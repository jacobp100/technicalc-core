/* eslint-disable no-console */
// Android font: GFS Didot
const fs = require("fs");
const path = require("path");
const { TeX } = require("mathjax-full/js/input/tex.js");
const { SVG } = require("mathjax-full/js/output/svg.js");
const {
  HTMLDocument,
} = require("mathjax-full/js/handlers/html/HTMLDocument.js");
const { liteAdaptor } = require("mathjax-full/js/adaptors/liteAdaptor.js");

const { AllPackages } = require("mathjax-full/js/input/tex/AllPackages.js");

const MmlVisitor = require("mathjax-full/js/core/MmlTree/SerializedMmlVisitor.js")
  .SerializedMmlVisitor;

const { Value } = require("../dist/client");
const titles = require("./titles");

const Typeset = (string, display) => {
  const tex = new TeX({ packages: AllPackages.sort() });
  const svg = new SVG();

  const html = new HTMLDocument("", liteAdaptor(), {
    InputJax: tex,
    OutputJax: svg,
  });

  const visitor = new MmlVisitor();
  const toMathML = (node) => visitor.visitTree(node, html);

  const math = new html.options.MathItem(string, tex, display);
  math.setMetrics(16, 16, 80, 100000, 1);
  math.compile(html);
  math.typeset(html);

  const out = toMathML(math.root)
    .replace(/>[\s\n]*</gm, "><")
    .replace(/\s?class="[^"]*"/g, "");
  return out;
};

const unnestDocument = (d) => d.replace(/<\/?math[^>]*>/g, "");

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

const data = require("./data");

const nist = fs
  .readFileSync(path.join(__dirname, "/data.csv"), "utf8")
  .split("\n")
  .filter((l) => l)
  .map((l) =>
    l
      .match(/^("[^"]+"|[^,]+),([^,]+)\s*,([^,]+)$/)
      .slice(1)
      .map((p) => p.replace(/^"/, "").replace(/"$/, "").trim())
  )
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

    let valueUtf = Value.toUnicode(Value.ofString(value));

    if (units) {
      const unitsUtf = units
        .replace("ohm", "Ω")
        .replace(/([_^])([-\w]+)/g, (_, subSuper, subSuperArg) => {
          const map =
            subSuper === "^" ? superscriptReplacements : subscriptReplacements;
          return Array.from(subSuperArg, (x) => {
            const val = map[x];
            if (val == null)
              throw new Error(`No mapping for ${x} (${valueUtf})`);
            return val;
          }).join("");
        });
      valueUtf += ` ${unitsUtf}`;
    }

    if (valueUtf.includes("^") || valueUtf.includes("_")) {
      throw new Error(`Invalid valueUtf (${valueUtf})`);
    }

    return { title, value, valueMml, valueUtf };
  });

const formats = [
  ["\\AA", "\\unicode{x212B}"],
  ["\\hbar", "\\unicode{x127}"],
  ["\\lbar", "\\unicode{x19b}"],
  ["\\lambdabar", "\\unicode{x19b}"],
  ["\\unicode{x212B}", "A"],
  ["\\unicode{x19b}", "\\lambda"],
];

let out = data.map(({ title: baseTitle, tex }) => {
  let formattedTex = tex;
  formattedTex = formattedTex.replace(
    /(?:\$([^$]*)\$|([^$,]+))/g,
    (full, texString, rawString) => {
      if (texString) return texString;
      if (rawString) return `{\\rm ${rawString}}`;
      throw new Error("Oh");
    }
  );
  // Take first if comma group (like c, c_0) UNLESS the comma is in a _{} or ^ group
  formattedTex = formattedTex
    .match(/^((?:[^{,_^]|[_^][^{]|[_^]?\{(?:[^}]|\{[^}]*\})*\})*).*$/)[1]
    .trim();
  formattedTex = formats.reduce(
    (accum, [a, b]) => accum.replace(a, b),
    formattedTex
  );

  if (baseTitle === "magnetic flux quantum") {
    // We would have to load a whole font just for this entry
    // Just change the formatting
    formattedTex = "\\Phi_0";
  }

  const normalizeTitle = (a) =>
    a
      .replace(/\s/g, "")
      .toLowerCase()
      .replace("moment", "mom.")
      .replace("magnetic", "mag.")
      .trim();

  const { value, valueUtf } = nist.find(
    (item) => normalizeTitle(baseTitle) === normalizeTitle(item.title)
  );
  let symbolMml;

  try {
    symbolMml = unnestDocument(Typeset(formattedTex, true));
    // Remove mrows that only contain one element
    symbolMml = symbolMml.replace(
      /<mrow>(<(\w+)[^>]*>[^<]*<\/\2>)<\/mrow>/g,
      "$1"
    );

    if (symbolMml.includes("merror")) {
      throw new Error(`Invalid tex ${tex}`);
    }
  } catch (e) {
    console.log("FAILED");
    console.log(e);
    console.log(baseTitle);
    console.log(formattedTex);
    throw e;
  }

  const title = baseTitle[0].toUpperCase() + baseTitle.slice(1);

  return { title, value, symbolMml, valueUtf };
});

out = out
  .filter((t) => !/in [KMG]?eV/.test(t.title))
  .filter((t) => {
    const filter = titles.get(t.title);
    if (filter == null) {
      throw new Error(`Missing filter for ${t.title}`);
    }
    return filter;
  });

const test = new Map();
out.forEach(({ title, symbolMml }) => {
  const existing = test.get(symbolMml) || [];
  existing.push(title);
  test.set(symbolMml, existing);
});

// const conflicts = Array.from(test.values()).filter(
//   (values) => values.length > 0
// );

fs.writeFileSync(
  path.resolve(__dirname, "../dist/constants.json"),
  JSON.stringify(out)
);

export {};
