module.exports = {
  env: {
    browser: true,
    es2021: true,
    node: true,
    jest: true,
  },
  parserOptions: {
    ecmaVersion: 12,
    sourceType: "module",
  },
  extends: ["airbnb-base", "prettier"],
  ignorePatterns: [
    "node_modules/",
    "**/node_modules/",
    "**/dist",
    "packages/technicalc-calculator/decimal",
    "packages/technicalc-prebuilt/mathjax-stubs",
    "**/*.bs.js",
  ],
  rules: {
    "import/no-extraneous-dependencies": [0],
  },
};
