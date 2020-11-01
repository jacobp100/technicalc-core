// For a detailed explanation regarding each configuration property, visit:
// https://jestjs.io/docs/en/configuration.html

module.exports = {
  reporters: [["jest-silent-reporter", { useDots: true }]],
  collectCoverageFrom: ["src/**/*.bs.js", "!src/__mocks__/**/*.js"],
  transformIgnorePatterns: ["/node_modules/(?!(bs-platform)/)"],
  testPathIgnorePatterns: ["/node_modules/", "<rootDir>/decimal"],
  transform: {
    "^.+\\.bs\\.js$": "babel-jest",
    "^.+\\.js$": "babel-jest",
  },
};
