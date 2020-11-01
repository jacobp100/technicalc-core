module.exports = {
  projects: [
    "<rootDir>/packages/technicalc-calculator",
    "<rootDir>/packages/technicalc-editor",
  ],
  transformIgnorePatterns: ["**/node_modules/(?!(bs-platform)/)"],
  testPathIgnorePatterns: [
    "/node_modules/",
    "**/node_modules/",
    "<rootDir>/packages/technicalc-calculator/decimal",
  ],
  // transform: {
  //   "^.+\\.bs\\.js$": "babel-jest",
  //   "^.+\\.js$": "babel-jest",
  // },
};
