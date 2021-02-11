const commonConfig = {
  transform: { "\\.m?[jt]sx?$": "babel-jest" },
  transformIgnorePatterns: ["node_modules/(?!(bs-platform)/)"],
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
