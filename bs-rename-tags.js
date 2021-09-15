// Currently only runs for the prod build
// Set this as `js-post-build` in bs-config to run tests with this enabled
import * as path from "path";
import * as fs from "fs";
import renameTags from "./packages/technicalc-prebuilt/rename-tags.js";

const inputPath = path.join(process.cwd(), process.argv[2]);

const contents = fs.readFileSync(inputPath, "utf8");
fs.writeFileSync(inputPath, renameTags(contents));
