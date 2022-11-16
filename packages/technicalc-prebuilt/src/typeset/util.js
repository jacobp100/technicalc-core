/* eslint-disable operator-assignment */
export const toNumberDefaultUndefined = (x) =>
  x !== "" ? Number(x) : undefined;

export const parseViewbox = (input) => {
  let [, y, width, height] = input.split(" ").map(Number);
  y = Math.min(y * 1e-3, -0.8);
  width = width * 1e-3;
  height = Math.max(height * 1e-3, 1);
  const ascent = -y;
  return { width, height, ascent };
};

export const parseTransform = (str) => {
  const out = { s: 1, tX: 0, tY: 0 };

  if (!str) return out;

  const re = /(\w+)\s*\(\s*(-?[\d.]*)\s*,?\s*(-?[\d.]*)\s*\)/gi;
  let match;
  // eslint-disable-next-line no-cond-assign
  while ((match = re.exec(str))) {
    switch (match[1]) {
      case "translate":
        out.tX += Number(match[2]);
        out.tY += Number(match[3]);
        break;
      case "scale":
        out.s *= Number(match[2]);
        break;
      default:
        throw new Error(`Unknown transform ${match[1]}`);
    }
  }

  return out;
};

const idRegExp = /^(~)?(\d+):(\d+)$/;

export const parseId = (id) => {
  const idMatch = id != null ? id.match(idRegExp) : null;

  if (idMatch == null) {
    return {
      current: undefined,
      after: undefined,
      avoidsSelection: false,
    };
  }

  const { 1: avoidsSelection, 2: current, 3: after } = idMatch;

  return {
    avoidsSelection: avoidsSelection != null,
    current: toNumberDefaultUndefined(current),
    after: toNumberDefaultUndefined(after),
  };
};

export const combineTransforms = (a, b) => ({
  s: a.s * b.s,
  tX: a.tX + b.tX * a.s,
  tY: a.tY + b.tY * a.s,
});
