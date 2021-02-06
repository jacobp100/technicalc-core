const fs = require("fs");
const path = require("path");
const util = require("util");
const childProcess = require("child_process");
const esbuild = require("esbuild");
const { minify } = require("terser");
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

const execFile = util.promisify(childProcess.execFile);

const build = (file, outfile) =>
  esbuild
    .build({
      entryPoints: [file],
      format: "cjs",
      bundle: true,
      minify: true,
      write: false,
      external: ["react", "react-native-svg"],
      plugins: [minifyDecimalJsPlugin, stubMathJaxPlugin],
    })
    .then((result) => {
      const code = result.outputFiles[0].text;
      return fast ? { code } : minify(code);
    })
    .then((result) => {
      return fs.promises.writeFile(path.join(dist, outfile), result.code);
    });

execFile("node", ["constants"]);
execFile("node", ["units"]);

const runFontGen = () => execFile("node", ["font-gen"]);

const fontGen = fast
  ? Promise.all([
      fs.promises.stat(path.join(__dirname, "fonts-assets")),
      fs.promises.stat(path.join(__dirname, "fonts-stubs")),
    ]).catch(() => {
      return runFontGen();
    })
  : runFontGen();

fontGen.then(() => {
  build("./src/Client.bs.js", "client.js");
  build("./src/Worker.bs.js", "worker.js");
  build("./src/typeset/index.js", "typeset.js");
});
