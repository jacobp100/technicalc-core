import { promises as fs } from "node:fs";
import path from "node:path";
import util from "node:util";
import childProcess from "node:child_process";
import { createRequire } from "module";
import esbuild from "esbuild";
import * as terser from "terser";

const dist = new URL("../../dist/", import.meta.url);
const stubsDir = new URL("stubs/", import.meta.url);

const require = createRequire(import.meta.url);

const evalPlugin = {
  name: "evalPlugin",
  setup(build) {
    build.onLoad({ filter: /_Eval/ }, async (args) => {
      const out = await import(args.path);
      const contents = Object.entries(out)
        .map(([key, value]) => {
          const valueJson = JSON.stringify(value);
          const valueFast = `JSON.parse('${valueJson.replace(/'/g, "\\'")}')`;
          return `export const ${key} = ${valueFast}`;
        })
        .join("\n");
      return { contents };
    });
  },
};

const decimalJsLightPlugin = {
  name: "decimalJsLightPlugin",
  setup(build) {
    build.onResolve({ filter: /decimal\.js$/ }, () => {
      return { path: require.resolve("decimal.js-light") };
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
      "../tex/"
    );

    const fontsStubsDir = new URL(".fonts/", stubsDir);
    build.onResolve({ filter: /tex[\\/]/i }, (args) => {
      const url = path.resolve(args.resolveDir, args.path).startsWith(texDir)
        ? new URL(path.basename(args.path), fontsStubsDir)
        : null;
      return url != null ? { path: url.pathname } : null;
    });

    const entitiesPath = require.resolve("mathjax-full/js/util/Entities");

    build.onResolve({ filter: /Entities.js$/i }, (args) => {
      return path.resolve(args.resolveDir, args.path) === entitiesPath
        ? { path: new URL("mathjax-full/entities.js", stubsDir).pathname }
        : null;
    });

    const wrapperPath = require.resolve("mathjax-full/js/output/svg/Wrapper");

    build.onResolve({ filter: /Wrapper.js$/i }, (args) => {
      return path.resolve(args.resolveDir, args.path) === wrapperPath
        ? { path: new URL("mathjax-full/Wrapper.js", stubsDir).pathname }
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
    code = code.replace(/export\s*\{([^{}]+)\}/, (_, inner) => {
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
const runNodeScript = async (filename, args) => {
  const run = () =>
    execFile("node", [filename, ...args.map((arg) => arg.pathname)]);

  if (fast) {
    try {
      const argsStat = args.map((f) => fs.stat(f));
      await Promise.all(argsStat);
    } catch {
      await run();
    }
  } else {
    await run();
  }
};

runNodeScript("scripts/constants", [new URL("constants.json", dist)]);
runNodeScript("scripts/units", [new URL("units.json", dist)]);

build({
  entryPoints: [new URL("src/Client.mjs", import.meta.url).pathname],
  outfile: new URL("client.js", dist),
  format: "umd",
  globalName: "Client",
  plugins: [decimalJsLightPlugin, minifyDecimalJsPlugin, evalPlugin],
});
build({
  entryPoints: [new URL("src/Worker.mjs", import.meta.url).pathname],
  outfile: new URL("worker.js", dist),
  format: "umd",
  globalName: "Worker",
  plugins: [minifyDecimalJsPlugin],
});

runNodeScript("scripts/fonts", [
  new URL("fonts", dist),
  new URL(".fonts", stubsDir),
]).then(() => {
  build({
    entryPoints: [new URL("src/typeset/index.js", import.meta.url).pathname],
    outfile: new URL("typeset.js", dist),
    format: "esm",
    external: ["react", "react-native-svg"],
    plugins: [stubMathJaxPlugin],
  });
});
