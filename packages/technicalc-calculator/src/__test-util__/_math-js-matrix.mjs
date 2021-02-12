import * as mathjs from "mathjs";
import { matrixOfFloats } from "../ValueTestUtil";
import cartesian from "./_cartesian";
import { Value } from "./_index";

export const matrix2x2 = cartesian([
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

export const matrix3x3 = cartesian([
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
