import fs from "node:fs";
import { boldItalic } from "mathjax-full/js/output/svg/fonts/tex/bold-italic.js";
import { bold } from "mathjax-full/js/output/svg/fonts/tex/bold.js";
import { italic } from "mathjax-full/js/output/svg/fonts/tex/italic.js";
import { largeop } from "mathjax-full/js/output/svg/fonts/tex/largeop.js";
import { normal } from "mathjax-full/js/output/svg/fonts/tex/normal.js";
import { smallop } from "mathjax-full/js/output/svg/fonts/tex/smallop.js";
import { texSize3 } from "mathjax-full/js/output/svg/fonts/tex/tex-size3.js";
import { texSize4 } from "mathjax-full/js/output/svg/fonts/tex/tex-size4.js";
import dist from "../../dist.mjs";
import { skewX } from "./font-util.mjs";
import buildFont from "./build-font.mjs";

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

const fontsStubs = new URL("../../stubs/.fonts/", import.meta.url);
const fontsAssetsPath = new URL("fonts/", dist);

ensureDir(fontsStubs);
ensureDir(fontsAssetsPath);

const buildJsonFont = (font, { preserveSvgChars }) => {
  Object.keys(font).forEach((key) => {
    const keepChar = Array.isArray(preserveSvgChars)
      ? preserveSvgChars.includes(+key)
      : preserveSvgChars ?? false;

    const data = font[key][3];
    if (!keepChar && data != null && data.p != null) {
      data.p = 0;
    }
  });

  return JSON.stringify(font);
};

const camelCase = (s) => s.replace(/-(\w)/g, (_, c) => c.toUpperCase());

const writeFont = (font, name, options = {}) => {
  if (options.preserveSvgChars !== true) {
    fs.writeFileSync(
      new URL(`mathjax-${name}.otf`, fontsAssetsPath),
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
    new URL(`${stubName}.js`, fontsStubs),
    `export const ${exportName} = JSON.parse('${json}');\n`
  );
};

const verticalBracketExtensions = [
  // Parentheses
  0x239c,
  0x239f,
  // Square brackets
  0x23a2,
  0x23a5,
  // Square root
  0xe000,
  0xe001,
];

writeFont(boldItalic, "bold-italic");
writeFont(bold, "bold");
writeFont(italic, "italic");
writeFont(normal, "normal", {
  // abs and d/dx with large content
  preserveSvgChars: [0x2223],
});
writeFont(largeop, "largeop", {
  preserveSvgChars: verticalBracketExtensions,
});
writeFont(smallop, "smallop", {
  preserveSvgChars: verticalBracketExtensions,
});
writeFont(texSize3, "size3", {
  preserveSvgChars: verticalBracketExtensions,
});
writeFont(texSize4, "size4", {
  preserveSvgChars: verticalBracketExtensions,
});

const stubFont = (name) => {
  const exportName = camelCase(name);

  fs.writeFileSync(
    new URL(`${name}.js`, fontsStubs),
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
