import * as mathjs from "mathjs/lib/esm";
import * as TechniCalc from "../Value";
import { toMatchJsValue } from "../__test-util__/_index";
import { complexValues } from "../__test-util__/_math-js";
import cartesian from "../__test-util__/_cartesian";

expect.extend({ toMatchJsValue });

const values = cartesian([complexValues, complexValues]);

test("add", () => {
  values.forEach(([a, b]) => {
    const actual = TechniCalc.add(a.techniCalcValue, b.techniCalcValue);
    const expected = mathjs.add(a.jsValue, b.jsValue);
    expect(actual).toMatchJsValue(expected, () => `${a} + ${b}`);
  });
});

test("sub", () => {
  values.forEach(([a, b]) => {
    const actual = TechniCalc.sub(a.techniCalcValue, b.techniCalcValue);
    const expected = mathjs.subtract(a.jsValue, b.jsValue);
    expect(actual).toMatchJsValue(expected, () => `${a} - ${b}`);
  });
});

test("mul", () => {
  values.forEach(([a, b]) => {
    const actual = TechniCalc.mul(a.techniCalcValue, b.techniCalcValue);
    const expected = mathjs.multiply(a.jsValue, b.jsValue);
    expect(actual).toMatchJsValue(expected, () => `${a} * ${b}`);
  });
});

test("div", () => {
  values.forEach(([a, b]) => {
    const actual = TechniCalc.div(a.techniCalcValue, b.techniCalcValue);
    const expected = mathjs.divide(a.jsValue, b.jsValue);
    expect(actual).toMatchJsValue(expected, () => `${a} / ${b}`);
  });
});
