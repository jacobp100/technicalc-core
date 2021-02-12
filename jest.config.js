const commonConfig = {
  testRegex: "(/__tests__/.*|(\\.|/)(test|spec))\\.([jt]sx?|mjs)$",
  moduleFileExtensions: ["mjs", "js"],
  transform: { "\\.m?[jt]sx?$": "babel-jest" },
  transformIgnorePatterns: ["node_modules/(?!(bs-platform|decimal.js)/)"],
};

module.exports = {
  projects: [
    {
      displayName: "technicalc-calculator",
      rootDir: "<rootDir>/packages/technicalc-calculator",
      ...commonConfig,
      testPathIgnorePatterns: ["/decimal"],
    },
    {
      displayName: "technicalc-editor",
      rootDir: "<rootDir>/packages/technicalc-editor",
      ...commonConfig,
    },
  ],
};
