const path = require("path");
const webpack = require("webpack");
const dist = require("./dist");

const createConfig = ({ name, entry, target = "web" }) => ({
  mode: "production",
  entry,
  output: {
    filename: `${name.toLowerCase()}.js`,
    path: dist,
    library: name,
    libraryTarget: "umd",
    globalObject: "typeof self !== 'undefined' ? self : undefined",
    publicPath: "/assets/",
  },
  target,
  externals: ["react", /^react-native-svg/],
  module: {
    rules: [
      // Reduce precision of built-in constants in decimal.js
      // From the default of 1000 to 100
      {
        test: /decimal\.js/,
        loader: "string-replace-loader",
        options: {
          search: /(\d\.\d{99})\d+/g,
          replace: "$1",
        },
      },
    ],
  },
  resolve: {
    alias: {
      "bs-platform": path.resolve(__dirname, "node_modules/bs-platform"),
    },
  },
  plugins: [
    new webpack.NormalModuleReplacementPlugin(
      /svg[\\/]fonts[\\/]tex[\\/].*\.js$/,
      (result) => {
        // https://github.com/webpack/webpack/blob/master/lib/NormalModuleReplacementPlugin.js
        const basename = path.basename(result.request);
        const file = require.resolve(`./fonts-stubs/${basename}`);
        // eslint-disable-next-line no-param-reassign
        result.request = file;
        if (result.createData != null) {
          // eslint-disable-next-line no-param-reassign
          result.createData.resource = file;
        }
      }
    ),
    new webpack.NormalModuleReplacementPlugin(
      /util[\\/]Entities.js$/,
      require.resolve("./mathjax-stubs/entities")
    ),
    new webpack.NormalModuleReplacementPlugin(
      /[\\/]output[\\/]svg[\\/]Wrapper.js$/,
      require.resolve("./mathjax-stubs/Wrapper")
    ),
  ],
});

module.exports = [
  createConfig({
    name: "Client",
    entry: "./src/Client.bs.js",
  }),
  createConfig({
    name: "Worker",
    entry: "./src/Worker.bs.js",
  }),
  createConfig({
    name: "Typeset",
    entry: "./src/typeset/index.js",
    target: "node",
  }),
];
