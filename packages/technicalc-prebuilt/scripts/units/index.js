import fs from "node:fs";
import { json } from "../../src/Units_Json.mjs";

const outputFilename = process.argv[2];

fs.writeFileSync(outputFilename, JSON.stringify(json));
