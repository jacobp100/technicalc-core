/* eslint-disable no-console */
// Android font: GFS Didot
import fs from "node:fs";
import { flatten } from "../../../technicalc-calculator/src/Units/Units.mjs";
import { Value, $$Symbol } from "../../src/Client.mjs";
import { unitsReplacements } from "../../src/Units_Constants.mjs";
import titles from "./titles.js";

const outputFilename = process.argv[2];

const data = JSON.parse(
  fs.readFileSync(new URL("data.json", import.meta.url), "utf8")
);

const symbols = JSON.parse(
  fs.readFileSync(new URL("symbols.json", import.meta.url), "utf8")
);

const ignored = new Set(["Molar Planck constant"]);

const normalizeTitle = (a) =>
  a
    .replace(/\s/g, "")
    .toLowerCase()
    .replace("moment", "mom.")
    .replace("magnetic", "mag.")
    .trim();

const nist = fs
  .readFileSync(new URL("data.csv", import.meta.url), "utf8")
  .split("\n")
  .filter((l) => l)
  .map((l) => {
    return l
      .match(/^("[^"]+"|[^,]+),([^,]+)\s*,([^,]+)$/)
      .slice(1)
      .map((p) => p.replace(/^"/, "").replace(/"$/, "").trim());
  })
  .map(([title, valueUnformatted, unitsString]) => {
    if (valueUnformatted == null) throw new Error("Oh");

    const value = valueUnformatted.replace(/\s/g, "").replace("...", "");
    const valueMml = Value.toMml(
      Value.ofString(value),
      { style: "decimal" },
      true /* inline */
    );

    if (valueMml.includes("NaN")) {
      throw new Error(`Invalid MML for ${title}`);
    }

    let units = [];
    if (unitsString) {
      unitsString.split(" ").forEach((str) => {
        const [unitBase, power = "1"] = str.split("^");

        const unitReplacement = unitsReplacements[unitBase];
        if (unitReplacement == null) {
          throw new Error("No unit defined");
        }

        unitReplacement.forEach((unit) => {
          units.push({ ...unit, power: unit.power * power });
        });
      });
    }
    units = flatten(units);

    const titleNormalized = normalizeTitle(title);

    return { titleNormalized, value, units };
  });

let out = data.map(({ title: baseTitle }) => {
  const baseTitleNormalized = normalizeTitle(baseTitle);
  const { value, units } = nist.find((item) => {
    return baseTitleNormalized === item.titleNormalized;
  });

  const title = baseTitle[0].toUpperCase() + baseTitle.slice(1);

  const symbolJson = symbols[title];
  const symbol =
    symbolJson != null && symbolJson.base !== ""
      ? $$Symbol.encode(symbolJson)
      : null;

  return { title, value, symbol, units };
});

out = out
  .filter((t) => !/in [KMG]?eV/.test(t.title))
  .filter((t) => !/^Inverse /.test(t.title))
  .filter((t) => {
    if (ignored.has(t.title)) {
      return false;
    }

    const filter = titles.get(t.title);
    if (filter == null) {
      throw new Error(`Missing filter for ${t.title}`);
    }

    return filter;
  });

out.forEach((t) => {
  if (t.symbol == null) {
    throw new Error(`Missing symbol for ${t.title}`);
  }
});

fs.writeFileSync(outputFilename, JSON.stringify(out));
