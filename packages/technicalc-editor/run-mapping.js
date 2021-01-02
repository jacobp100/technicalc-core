const path = require("path");

const input = path.relative(
  __dirname,
  path.join(process.cwd(), process.argv[2])
);

const output = new Map([
  [
    "src/Encoding/Encoding_BuildElementMapping.bs.js",
    "src/Encoding/Encoding_ElementMapping.js",
  ],
  [
    "src/Encoding/Encoding_BuildUnitMapping.bs.js",
    "src/Encoding/Encoding_UnitMapping.js",
  ],
  [
    "src/Encoding/Encoding_BuildPrefixMapping.bs.js",
    "src/Encoding/Encoding_PrefixMapping.js",
  ],
]).get(input);

if (output != null) {
  // Lazy load modules for perf
  // Since this is is run for every build of every file in this module
  /* eslint-disable global-require */
  const fs = require("fs");
  const requireEsm = require("esm")(module);

  const { mapping, reverseMapping } = requireEsm(path.join(__dirname, input));

  const js = [
    `export const mapping = ${JSON.stringify(mapping)};`,
    `export const reverseMapping = ${JSON.stringify(reverseMapping)};`,
  ].join("\n");

  fs.writeFileSync(path.join(__dirname, output), js);
}
