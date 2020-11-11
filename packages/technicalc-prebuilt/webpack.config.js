const path = require("path");
const webpack = require("webpack");

const createConfig = ({ name, entry, outputDirectory, target = "web" }) => ({
  mode: "production",
  entry,
  output: {
    filename: `${name.toLowerCase()}.js`,
    path: path.resolve(__dirname, outputDirectory),
    library: name,
    libraryTarget: "umd",
    globalObject: "typeof self !== 'undefined' ? self : undefined",
    publicPath: "/assets/",
  },
  target,
  externals: ["react", /^react-native-svg/],
  resolve: {
    alias: {
      "bs-platform": path.resolve(__dirname, "node_modules/bs-platform"),
    },
  },
  plugins: [
    new webpack.NormalModuleReplacementPlugin(
      /svg[\\/]fonts[\\/]tex[\\/].*.js$/,
      require.resolve("./mathjax-stubs/fonts")
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
    outputDirectory: "dist",
    entry: "./src/Client.bs.js",
  }),
  createConfig({
    name: "Worker",
    outputDirectory: "dist",
    entry: "./src/Worker.bs.js",
  }),
  createConfig({
    name: "Typeset",
    outputDirectory: "dist",
    entry: "./src/typeset/index.js",
    target: "node",
  }),
];
