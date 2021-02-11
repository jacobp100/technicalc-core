const requireEsm = require("esm")(module);
const fs = require("fs").promises;
const path = require("path");
const util = require("util");
const childProcess = require("child_process");
const esbuild = require("esbuild");
const terser = require("terser");
const dist = require("./dist");

const fontsStubsDir = path.resolve(__dirname, "./stubs/.fonts");

const evalPlugin = {
  name: "evalPlugin",
  setup(build) {
    build.onLoad({ filter: /_Eval/ }, (args) => {
      const out = requireEsm(args.path);
      const contents = Object.entries(out)
        .map(([key, value]) => {
          const valueJson = JSON.stringify(value);
          const valueFast = `JSON.parse('${valueJson.replace(/'/g, "\\")}')`;
          return `export const ${key} = ${valueFast}`;
        })
        .join("\n");
      return { contents };
    });
  },
};

const minifyDecimalJsPlugin = {
  name: "minifyDecimalJsPlugin",
  setup(build) {
    build.onLoad({ filter: /decimal\.js$/ }, async (args) => {
      const text = await fs.readFile(args.path, "utf8");
      const contents = text.replace(/(\d\.\d{99})\d+/g, "$1");
      return { contents };
    });
  },
};

const stubMathJaxPlugin = {
  name: "stubMathJaxPlugin",
  setup(build) {
    const texDir = path.resolve(
      require.resolve("mathjax-full/js/output/svg/fonts/tex"),
      "../tex"
    );

    build.onResolve({ filter: /tex[\\/]/i }, (args) => {
      return path.resolve(args.resolveDir, args.path).startsWith(texDir)
        ? { path: path.resolve(fontsStubsDir, path.basename(args.path)) }
        : null;
    });

    const entitiesPath = require.resolve("mathjax-full/js/util/Entities");

    build.onResolve({ filter: /Entities.js$/i }, (args) => {
      return path.resolve(args.resolveDir, args.path) === entitiesPath
        ? { path: require.resolve("./stubs/mathjax-full/entities") }
        : null;
    });

    const wrapperPath = require.resolve("mathjax-full/js/output/svg/Wrapper");

    build.onResolve({ filter: /Wrapper.js$/i }, (args) => {
      return path.resolve(args.resolveDir, args.path) === wrapperPath
        ? { path: require.resolve("./stubs/mathjax-full/Wrapper") }
        : null;
    });
  },
};

const fast = process.argv.includes("--fast");

const build = async ({ outfile, format, globalName, ...rest }) => {
  const { outputFiles } = await esbuild.build({
    bundle: true,
    minify: true,
    write: false,
    ...rest,
    format: format === "umd" ? "esm" : format,
    globalName: format === "umd" ? undefined : globalName,
  });

  let code = outputFiles[0].text;

  if (format === "umd") {
    // HACK: convert to UMD - only supports cjs and global var
    const varName = "__EXPORTS__";
    code = code
      .replace(/import\s+(\w+)\s+from\s*"([^"]+)"/g, 'var $1 = require("$2")')
      .replace(/export\s*\{([^{}]+)\}/, (_, inner) => {
        const defaultExport = inner.match(/^(\w+) as default$/);
        return defaultExport != null
          ? `var ${varName}=${defaultExport[1]}`
          : `var ${varName}={${inner.replace(/(\w+) as (\w+)/g, "$2:$1")}}`;
      });
    code = `(()=>{${code};typeof module!=='undefined'?module.exports=${varName}:self.${globalName}=${varName}})()`;
  }

  if (!fast) {
    code = (await terser.minify(code)).code;
  }

  await fs.writeFile(outfile, code);
};

const execFile = util.promisify(childProcess.execFile);
const runNodeScript = async (filename, dependencies) => {
  const run = () => execFile("node", [filename]);

  if (fast) {
    try {
      const dependenciesStat = dependencies.map((f) => fs.stat(f));
      await Promise.all(dependenciesStat);
    } catch {
      await run();
    }
  } else {
    await run();
  }
};

runNodeScript("scripts/constants", [path.resolve(dist, "constants.json")]);
runNodeScript("scripts/units", [path.resolve(dist, "units.json")]);

build({
  entryPoints: ["./src/Client.bs.js"],
  outfile: path.resolve(dist, "client.js"),
  format: "umd",
  globalName: "Client",
  plugins: [minifyDecimalJsPlugin, evalPlugin],
});
build({
  entryPoints: ["./src/Worker.bs.js"],
  outfile: path.resolve(dist, "worker.js"),
  format: "umd",
  globalName: "Worker",
  plugins: [minifyDecimalJsPlugin],
});

runNodeScript("scripts/fonts", [
  fontsStubsDir,
  path.resolve(dist, "fonts"),
]).then(() => {
  build({
    entryPoints: ["./src/typeset/index.js"],
    outfile: path.resolve(dist, "typeset.js"),
    format: "esm",
    external: ["react", "react-native-svg"],
    plugins: [stubMathJaxPlugin],
  });
});
