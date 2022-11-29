/* eslint-disable no-console */
// Android font: GFS Didot
import fs from "node:fs";
import { Value, $$Symbol } from "../../src/Client.mjs";
import titles from "./titles.js";

const outputFilename = process.argv[2];

const data = JSON.parse(
  fs.readFileSync(new URL("data.json", import.meta.url), "utf8")
);

const symbols = JSON.parse(
  fs.readFileSync(new URL("symbols.json", import.meta.url), "utf8")
);

const superscriptReplacements = {
  "-": "⁻",
  0: "⁰",
  1: "¹",
  2: "²",
  3: "³",
  4: "⁴",
  5: "⁵",
  6: "⁶",
  7: "⁷",
  8: "⁸",
  9: "⁹",
};

const subscriptReplacements = {
  0: "₀",
  1: "₁",
  2: "₂",
  3: "₃",
  4: "₄",
  5: "₅",
  6: "₆",
  7: "₇",
  8: "₈",
  9: "₉",
  h: "h",
};

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
  .map(([title, valueUnformatted, units]) => {
    if (valueUnformatted == null) throw new Error("Oh");

    const value = valueUnformatted.replace(/\s/g, "").replace("...", "");
    let valueMml = Value.toMml(
      Value.ofString(value),
      { style: "decimal" },
      true /* inline */
    );

    if (units) {
      const unitsMml = units
        .split(" ")
        .map((str) => {
          const [unitBase, power] = str.split("^");
          const [unit, base] = unitBase.split("_");
          const unitWithHtml = unit.replace("ohm", "&#x2126;");

          const unitNameMml = `<mi mathvariant="normal">${unitWithHtml}</mi>`;

          const baseTag = Number.isNaN(Number(base)) ? "mi" : "mn";
          const baseMml =
            base == null ? "" : `<${baseTag}>${base}</${baseTag}>`;
          const unitMml = baseMml
            ? `<msub>${unitNameMml}${baseMml}</msub>`
            : unitNameMml;

          return power ? `<msup>${unitMml}<mn>${power}</mn></msup>` : unitMml;
        })
        .join("<mo>&#183;</mo>");
      valueMml = valueMml.replace(
        "</math>",
        `<mspace width="8px">${unitsMml}</math>`
      );
    }

    if (valueMml.includes("NaN")) {
      throw new Error(`Invalid MML for ${title}`);
    }

    let unitsUtf = "";
    if (units) {
      unitsUtf = units
        .replace("ohm", "Ω")
        .replace(/([_^])([-\w]+)/g, (_, subSuper, subSuperArg) => {
          const map =
            subSuper === "^" ? superscriptReplacements : subscriptReplacements;
          return Array.from(subSuperArg, (x) => {
            const val = map[x];
            if (val == null) {
              throw new Error(`No mapping for ${x}`);
            }
            return val;
          }).join("");
        });
    }

    const titleNormalized = normalizeTitle(title);

    return { titleNormalized, value, valueMml, unitsUtf };
  });

let out = data.map(({ title: baseTitle }) => {
  const baseTitleNormalized = normalizeTitle(baseTitle);
  const { value, unitsUtf } = nist.find((item) => {
    return baseTitleNormalized === item.titleNormalized;
  });

  const title = baseTitle[0].toUpperCase() + baseTitle.slice(1);

  const symbolJson = symbols[title];
  const symbol =
    symbolJson != null && symbolJson.base !== ""
      ? $$Symbol.encode(symbolJson)
      : null;

  return { title, value, symbol, unitsUtf };
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
