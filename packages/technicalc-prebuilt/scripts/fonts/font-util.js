export const skewX = (char, deg) => {
  const c = Math.atan((deg * 2 * Math.PI) / 360);
  const data = char.slice();
  const p = data
    .pop()
    .p.replace(
      /(-?\d+)\s+(-?\d+)/g,
      (_, x, y) => `${(+x + +y * c).toFixed(0)} ${y}`
    );
  data.push({ p });
  return data;
};
