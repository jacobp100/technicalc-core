const fs = require("fs");
const path = require("path");
const requireEsm = require("esm")(module);
const dist = require("../dist");

const { json } = requireEsm("../src/Units_Json.bs.js");

fs.writeFileSync(path.join(dist, "units.json"), JSON.stringify(json));
