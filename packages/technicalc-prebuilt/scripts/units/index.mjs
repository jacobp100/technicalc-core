import fs from "node:fs";
import { json } from "../../src/Units_Json.mjs";
import dist from "../../dist.mjs";

fs.writeFileSync(new URL("units.json", dist), JSON.stringify(json));
