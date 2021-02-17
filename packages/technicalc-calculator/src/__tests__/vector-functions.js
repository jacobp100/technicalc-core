import * as mathjs from "mathjs/lib/esm";
import * as TechniCalc from "../Value";
import { toMatchJsValue, toMatchJsMatrix } from "../__test-util__/index";
import { vector2, vector3 } from "../__test-util__/math-js-vector";
import cartesian from "../__test-util__/cartesian";

expect.extend({ toMatchJsValue, toMatchJsMatrix });

test("cross", () => {
  cartesian([vector3, vector3]).forEach(([a, b]) => {
    const [i, j, k] = mathjs.cross(a.jsValue, b.jsValue);
    const mathJsValue = mathjs.matrix([[i], [j], [k]]);
    expect(
      TechniCalc.mul(a.techniCalcValue, b.techniCalcValue)
    ).toMatchJsMatrix(mathJsValue, () => `${a} * ${b}`);
  });
});

test("dot", () => {
  const values = [
    ...cartesian([vector2, vector2]),
    ...cartesian([vector3, vector3]),
  ];

  values.forEach(([a, b]) => {
    expect(TechniCalc.dot(a.techniCalcValue, b.techniCalcValue)).toMatchJsValue(
      mathjs.dot(a.jsValue, b.jsValue),
      `${a} . ${b}`
    );
  });

  expect(
    TechniCalc.dot(vector3[0].techniCalcValue, vector2[0].techniCalcValue)
  ).toMatchJsValue(NaN);
});

test(`det([3 ,7, 8])`, () => {
  expect(
    TechniCalc.abs(TechniCalc.ofVector([3, 7, 8].map(TechniCalc.ofFloat)))
  ).toMatchJsValue(Math.sqrt(122));
});
