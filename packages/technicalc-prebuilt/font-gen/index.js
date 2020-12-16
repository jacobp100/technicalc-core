const fs = require("fs");
const path = require("path");
const {
  boldItalic,
} = require("mathjax-full/js/output/svg/fonts/tex/bold-italic");
const { bold } = require("mathjax-full/js/output/svg/fonts/tex/bold");
const { italic } = require("mathjax-full/js/output/svg/fonts/tex/italic");
const { largeop } = require("mathjax-full/js/output/svg/fonts/tex/largeop");
const { normal } = require("mathjax-full/js/output/svg/fonts/tex/normal");
const { smallop } = require("mathjax-full/js/output/svg/fonts/tex/smallop");
const { texSize3 } = require("mathjax-full/js/output/svg/fonts/tex/tex-size3");
const { texSize4 } = require("mathjax-full/js/output/svg/fonts/tex/tex-size4");
const { skewX } = require("./font-util");
const buildFont = require("./build-font");

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

const fontsJsonPath = path.resolve(__dirname, "../fonts-json");
const fontsAssetsPath = path.resolve(__dirname, "../fonts-assets");

ensureDir(fontsJsonPath);
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

const writeFont = (font, name, options = {}) => {
  if (options.preserveSvgChars !== true) {
    fs.writeFileSync(
      path.join(fontsAssetsPath, `mathjax-${name}.otf`),
      buildFont({ familyName: `mathjax-${name}`, styleName: "Regular" }, font)
    );
  }

  fs.writeFileSync(
    path.join(fontsJsonPath, `${name}.json`),
    buildJsonFont(font, options)
  );
};

writeFont(boldItalic, "bold-italic");
writeFont(bold, "bold");
writeFont(italic, "italic");
writeFont(normal, "normal", {
  preserveSvgChars: [
    // abs and d/dx with large content
    0x2223,
  ],
});
writeFont(largeop, "largeop", { preserveSvgChars: true });
writeFont(smallop, "smallop", { preserveSvgChars: true });
writeFont(texSize3, "size3", { preserveSvgChars: true });
writeFont(texSize4, "size4", { preserveSvgChars: true });
