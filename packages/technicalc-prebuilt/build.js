const fs = require("fs");
const path = require("path");
const util = require("util");
const childProcess = require("child_process");
const esbuild = require("esbuild");
const terser = require("terser");
const dist = require("./dist");

const minifyDecimalJsPlugin = {
  name: "minifyDecimalJsPlugin",
  setup(build) {
    build.onLoad({ filter: /decimal\.js$/ }, async (args) => {
      const text = await fs.promises.readFile(args.path, "utf8");
      const contents = text.replace(/(\d\.\d{99})\d+/g, "$1");
      return { contents };
    });
  },
};

const stubMathJaxPlugin = {
  name: "stubMathJaxPlugin",
  setup(build) {
    const texPath = require.resolve("mathjax-full/js/output/svg/fonts/tex");
    const texDir = path.resolve(texPath, "../tex");
    const fontsStubs = path.resolve(__dirname, "fonts-stubs");

    build.onResolve({ filter: /tex[\\/]/i }, (args) => {
      return path.resolve(args.resolveDir, args.path).startsWith(texDir)
        ? { path: path.resolve(fontsStubs, path.basename(args.path)) }
        : null;
    });

    const entitiesPath = require.resolve("mathjax-full/js/util/Entities");

    build.onResolve({ filter: /Entities.js$/i }, (args) => {
      return path.resolve(args.resolveDir, args.path) === entitiesPath
        ? { path: require.resolve("./mathjax-stubs/entities.js") }
        : null;
    });

    const wrapperPath = require.resolve("mathjax-full/js/output/svg/Wrapper");

    build.onResolve({ filter: /Wrapper.js$/i }, (args) => {
      return path.resolve(args.resolveDir, args.path) === wrapperPath
        ? { path: require.resolve("./mathjax-stubs/Wrapper.js") }
        : null;
    });
  },
};

const fast = process.argv.includes("--fast");

const build = async (file, outfile) => {
  const { outputFiles } = await esbuild.build({
    entryPoints: [file],
    format: "cjs",
    bundle: true,
    minify: true,
    write: false,
    external: ["react", "react-native-svg"],
    plugins: [minifyDecimalJsPlugin, stubMathJaxPlugin],
  });

  let code = outputFiles[0].text;

  if (!fast) {
    code = (await terser.minify(code)).code;
  }

  await fs.promises.writeFile(path.join(dist, outfile), code);
};

const execFile = util.promisify(childProcess.execFile);
const run = async (filename, dependencies) => {
  const perform = () => execFile("node", [filename]);

  if (fast) {
    const dependenciesStat = dependencies.map((f) => fs.promises.stat(f));

    try {
      await Promise.all(dependenciesStat);
    } catch {
      perform();
    }
  } else {
    perform();
  }
};

run("constants", [path.resolve(dist, "constants.json")]);
run("units", [path.resolve(dist, "units.json")]);

run("font-gen", [
  path.join(__dirname, "fonts-assets"),
  path.join(__dirname, "fonts-stubs"),
]).then(() => {
  build("./src/Client.bs.js", "client.js");
  build("./src/Worker.bs.js", "worker.js");
  build("./src/typeset/index.js", "typeset.js");
});
