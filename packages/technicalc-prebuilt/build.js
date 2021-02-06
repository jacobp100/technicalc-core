const fs = require("fs");
const esbuild = require("esbuild");

const minifyDecimalJsPlugin = {
  name: "minifyDecimalJsPlugin",
  setup(build) {
    build.onLoad({ filter: /decimal\.js$/ }, async (args) => {
      const text = await fs.promises.readFile(args.path, "utf8");
      return {
        contents: text.replace(/(\d\.\d{99})\d+/g, "$1"),
      };
    });
  },
};

esbuild.build({
  entryPoints: ["./src/Client.bs.js"],
  bundle: true,
  minify: true,
  format: "cjs",
  outfile: "esbuild/client.js",
  plugins: [minifyDecimalJsPlugin],
});

esbuild.build({
  entryPoints: ["./src/Worker.bs.js"],
  bundle: true,
  minify: true,
  outfile: "esbuild/worker.js",
  plugins: [minifyDecimalJsPlugin],
});
