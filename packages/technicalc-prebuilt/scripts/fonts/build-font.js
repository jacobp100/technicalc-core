import { Font, Glyph, Path } from "opentype.js";

const pathRegexp =
  /([A-Z])\s*(-?[\d.]*)\s*,?\s*(-?[\d.]*)\s*,?\s*(-?[\d.]*)\s*,?\s*(-?[\d.]*)\s*,?\s*(-?[\d.]*)\s*,?\s*(-?[\d.]*)\s*/gi;

const pathDataToPath = (pathData) => {
  const path = new Path();

  const normalizedpathData = `M${pathData}Z`;
  let match;

  let x = 0;
  let y = 0;
  let qX = 0;
  let qY = 0;
  // eslint-disable-next-line no-cond-assign
  while ((match = pathRegexp.exec(normalizedpathData)) != null) {
    const [, command, a, b, c, d, e, f] = match;
    switch (command) {
      case "M":
        x = +a;
        y = +b;
        path.moveTo(x, y);
        break;
      case "L":
        x = +a;
        y = +b;
        path.lineTo(x, y);
        break;
      case "T":
        qX = 2 * x - qX;
        qY = 2 * y - qY;
        x = +a;
        y = +b;
        path.quadraticCurveTo(qX, qY, x, y);
        break;
      case "Q":
        qX = +a;
        qY = +b;
        x = +c;
        y = +d;
        path.quadraticCurveTo(qX, qY, x, y);
        break;
      case "C":
        x = +e;
        y = +f;
        path.bezierCurveTo(+a, +b, +c, +d, x, y);
        break;
      case "H":
        x = +a;
        path.lineTo(x, y);
        break;
      case "V":
        y = +a;
        path.lineTo(x, y);
        break;
      case "Z":
        path.close();
        break;
      default:
        throw new Error(`Unhandled path command ${command}`);
    }

    if (command !== "Q" && command !== "T") {
      qX = x;
      qY = y;
    }
  }

  return path;
};

export default ({ familyName, styleName }, chars) => {
  const unitsPerEm = 1000;
  let ascender = 0;
  let descender = 0;

  const glyphs = Object.entries(chars)
    .map(([unicodeString, data]) => {
      const unicode = +unicodeString;
      const [glyphAscender, glyphDescender, width, charData] = data;

      ascender = Math.max(ascender, glyphAscender * unitsPerEm);
      descender = Math.min(descender, -glyphDescender * unitsPerEm);

      const pathData = charData?.p;
      if (pathData == null || pathData.length === 0) {
        return null;
      }

      const advanceWidth = width * unitsPerEm;
      const path = pathDataToPath(pathData);

      const glyph = new Glyph({
        name: String.fromCharCode(unicode),
        unicode,
        advanceWidth,
        path,
      });

      return glyph;
    })
    .filter((glyph) => glyph !== null);

  glyphs.unshift(
    new Glyph({
      name: ".notdef",
      unicode: 0,
      advanceWidth: 0,
      path: new Path(),
    })
  );

  const font = new Font({
    familyName,
    styleName,
    unitsPerEm,
    ascender,
    descender,
    glyphs,
  });

  const buffer = Buffer.from(font.toArrayBuffer());
  return buffer;
};
