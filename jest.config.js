const commonConfig = {
  testRegex: "(/__tests__/.*|(\\.|/)(test|spec))\\.([jt]sx?|mjs)$",
  moduleFileExtensions: ["mjs", "js"],
  // https://jestjs.io/docs/en/ecmascript-modules
  transform: {},
};

export default {
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
