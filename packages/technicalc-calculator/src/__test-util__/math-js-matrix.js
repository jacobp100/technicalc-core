const mathjs = require("mathjs");
const { matrixOfFloats } = require("../ValueTestUtil.bs");
const cartesian = require("./cartesian");
const { Value } = require(".");

const matrix2x2 = cartesian([
  [0, -1, 1, 5],
  [0, -1, 1],
  [0, 1, 5],
  [0, -1, 1],
]).map(
  ([a, b, c, d]) =>
    new Value(
      mathjs.matrix([
        [a, b],
        [c, d],
      ]),
      matrixOfFloats(2, 2, [a, b, c, d]),
      `[[${a}, ${b}], [${c}, ${d}]]`
    )
);
module.exports.matrix2x2 = matrix2x2;

const matrix3x3 = cartesian([
  [0, -1],
  [0, 1],
  [0, 5],
  [0, 1],
  [0, -1],
  [0],
  [0, 5],
  [0, 1],
  [0],
]).map(
  ([a, b, c, d, e, f, g, h, i]) =>
    new Value(
      mathjs.matrix([
        [a, b, c],
        [d, e, f],
        [g, h, i],
      ]),
      matrixOfFloats(3, 3, [a, b, c, d, e, f, g, h, i]),
      `[[${a}, ${b}, ${c}], [${d}, ${e}, ${f}], [${g}, ${h}, ${i}]]`
    )
);
module.exports.matrix3x3 = matrix3x3;
