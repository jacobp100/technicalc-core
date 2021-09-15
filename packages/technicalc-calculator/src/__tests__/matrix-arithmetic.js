import * as mathjs from "mathjs/lib/esm";
import * as TechniCalc from "../Value/Value";
import * as TechniCalcTest from "../__test-util__/ValueTestUtil";
import { toMatchJsMatrix } from "../__test-util__/index";
import { matrix2x2, matrix3x3 } from "../__test-util__/math-js-matrix";
import cartesian from "../__test-util__/cartesian";

expect.extend({ toMatchJsMatrix });

const values2x2 = cartesian([matrix2x2, matrix2x2]);
const values3x3 = cartesian([matrix3x3, matrix3x3]);
const values = [...values2x2, ...values3x3];

test("add", () => {
  values.forEach(([a, b]) => {
    const actualValue = TechniCalc.add(a.techniCalcValue, b.techniCalcValue);
    const expectedValue = mathjs.add(a.jsValue, b.jsValue);
    expect(actualValue).toMatchJsMatrix(expectedValue, () => `${a} + ${b}`);
  });

  const nanValue = TechniCalc.add(
    matrix2x2[0].techniCalcValue,
    matrix3x3[0].techniCalcValue
  );
  expect(TechniCalcTest.toString(nanValue)).toBe("NaN");
});

test("sub", () => {
  values.forEach(([a, b]) => {
    const actualValue = TechniCalc.sub(a.techniCalcValue, b.techniCalcValue);
    const expectedValue = mathjs.subtract(a.jsValue, b.jsValue);
    expect(actualValue).toMatchJsMatrix(expectedValue, () => `${a} + ${b}`);
  });

  const nanValue = TechniCalc.sub(
    matrix2x2[0].techniCalcValue,
    matrix3x3[0].techniCalcValue
  );
  expect(TechniCalcTest.toString(nanValue)).toBe("NaN");
});

test("mul", () => {
  values.forEach(([a, b]) => {
    const actualValue = TechniCalc.mul(a.techniCalcValue, b.techniCalcValue);
    const expectedValue = mathjs.multiply(a.jsValue, b.jsValue);
    expect(actualValue).toMatchJsMatrix(expectedValue, () => `${a} + ${b}`);
  });

  const nanValue = TechniCalc.mul(
    matrix2x2[0].techniCalcValue,
    matrix3x3[0].techniCalcValue
  );
  expect(TechniCalcTest.toString(nanValue)).toBe("NaN");
});
