import fs from "node:fs";
import { json } from "../../src/Conversions_Json.mjs";

const outputFilename = process.argv[2];

fs.writeFileSync(outputFilename, JSON.stringify(json));
