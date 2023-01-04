import fs from "node:fs";
import path from "node:path";
import { boldItalic } from "mathjax-full/js/output/svg/fonts/tex/bold-italic.js";
import { bold } from "mathjax-full/js/output/svg/fonts/tex/bold.js";
import { italic } from "mathjax-full/js/output/svg/fonts/tex/italic.js";
import { largeop } from "mathjax-full/js/output/svg/fonts/tex/largeop.js";
import { normal } from "mathjax-full/js/output/svg/fonts/tex/normal.js";
import { smallop } from "mathjax-full/js/output/svg/fonts/tex/smallop.js";
import { texSize3 } from "mathjax-full/js/output/svg/fonts/tex/tex-size3.js";
import { texSize4 } from "mathjax-full/js/output/svg/fonts/tex/tex-size4.js";
import { skewX } from "./font-util.js";
import buildFont from "./build-font.js";

const fontsAssetsPath = process.argv[2];
const fontsStubs = process.argv[3];

// Add missing non-italic characters
for (let i = 0x3b1; i <= 0x3c9; i += 1) {
  normal[i] = skewX(italic[i], -15);
  bold[i] = skewX(boldItalic[i], -15);
}

const ensureDir = (dir) => {
  if (!fs.existsSync(dir)) {
    fs.mkdirSync(dir);
  }
};

ensureDir(fontsStubs);
ensureDir(fontsAssetsPath);

const buildJsonFont = (font) => {
  Object.keys(font).forEach((key) => {
    const data = font[key][3];
    if (data?.p != null) {
      data.p = 0;
    }
  });

  return JSON.stringify(font);
};

const camelCase = (s) => s.replace(/-(\w)/g, (_, c) => c.toUpperCase());

const writeFont = (font, name, options = {}) => {
  if (options.preserveSvgChars !== true) {
    fs.writeFileSync(
      path.join(fontsAssetsPath, `mathjax-${name}.otf`),
      buildFont({ familyName: `mathjax-${name}`, styleName: "Regular" }, font)
    );
  }

  const stubName = name.startsWith("size") ? `tex-${name}` : name;

  const exportName = camelCase(stubName);
  const json = buildJsonFont(font, options);

  if (json.includes("'")) {
    throw new Error("Invalid JSON");
  }

  fs.writeFileSync(
    path.join(fontsStubs, `${stubName}.js`),
    `export const ${exportName} = JSON.parse('${json}');\n`
  );
};

writeFont(boldItalic, "bold-italic");
writeFont(bold, "bold");
writeFont(italic, "italic");
writeFont(normal, "normal");
writeFont(largeop, "largeop");
writeFont(smallop, "smallop");
writeFont(texSize3, "size3");
writeFont(texSize4, "size4");

const stubFont = (name) => {
  const exportName = camelCase(name);

  fs.writeFileSync(
    path.join(fontsStubs, `${name}.js`),
    `export const ${exportName} = {};\n`
  );
};

stubFont("double-struck");
stubFont("fraktur-bold");
stubFont("fraktur");
stubFont("monospace");
stubFont("sans-serif-bold-italic");
stubFont("sans-serif-bold");
stubFont("sans-serif-italic");
stubFont("sans-serif");
stubFont("script-bold");
stubFont("script");
stubFont("tex-calligraphic-bold");
stubFont("tex-calligraphic");
stubFont("tex-mathit");
stubFont("tex-oldstyle-bold");
stubFont("tex-oldstyle");
stubFont("tex-variant");
