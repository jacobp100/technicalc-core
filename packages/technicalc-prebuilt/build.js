const fs = require("fs");
const path = require("path");
const esbuild = require("esbuild");
const { minify } = require("terser");

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
    const texPath = require.resolve("mathjax-full/js/output/svg/fonts/tex.js");
    const fontsStubs = path.resolve(__dirname, "fonts-stubs");

    build.onResolve({ filter: /tex[\\/]/i }, (args) => {
      const isSvgFontFile =
        args.importer === texPath && args.path.startsWith("./tex");

      return isSvgFontFile
        ? { path: path.resolve(fontsStubs, path.basename(args.path)) }
        : {};
    });

    build.onResolve({ filter: /util[\\/]Entities.js$/i }, () => ({
      path: path.resolve(__dirname, "mathjax-stubs/entities.js"),
    }));

    build.onResolve({ filter: /[\\/]output[\\/]svg[\\/]Wrapper.js$/i }, () => ({
      path: path.resolve(__dirname, "mathjax-stubs/Wrapper.js"),
    }));
  },
};

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
      return minify(result.outputFiles[0].text);
    })
    .then((result) => {
      return fs.promises.writeFile(
        path.join(__dirname, "esbuild", outfile),
        result.code
      );
    });

build("./src/Client.bs.js", "client.js");
build("./src/Worker.bs.js", "worker.js");
build("./src/typeset/index.js", "typeset.js");
