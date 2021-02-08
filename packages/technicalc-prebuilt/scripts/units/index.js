const requireEsm = require("esm")(module);
const fs = require("fs");
const path = require("path");

const { json } = requireEsm("../../src/Units_Json.bs.js");
const dist = require("../../dist");

fs.writeFileSync(path.join(dist, "units.json"), JSON.stringify(json));
