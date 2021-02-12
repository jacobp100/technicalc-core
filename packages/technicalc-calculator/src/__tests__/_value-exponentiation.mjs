import * as mathjs from "mathjs";
import { toMatchJsValue } from "../__test-util__/_index";
import { complexValues } from "../__test-util__/_math-js";
import * as TechniCalc from "../Value";
import * as TechniCalcTest from "../ValueTestUtil";

expect.extend({ toMatchJsValue });

test("exp", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.exp(v.techniCalcValue);
    const expected = mathjs.exp(v.jsValue);
    expect(actual).toMatchJsValue(expected, () => `exp(${v})`);
  });
});

test("log", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.log(v.techniCalcValue);
    const expected = mathjs.log(v.jsValue);

    expect(actual).toMatchJsValue(expected, () => `log(${v})`);
  });
});

test("special cases square root of negative numbers", () => {
  expect(TechniCalcTest.toString(TechniCalc.sqrt(TechniCalc.ofInt(-2)))).toBe(
    "sqrt(2)i"
  );
});

test("handles large numbers", () => {
  expect(
    TechniCalcTest.toString(TechniCalc.sqrt(TechniCalc.ofFloat(1e6)))
  ).toBe("1,000");
  expect(
    TechniCalcTest.toString(TechniCalc.sqrt(TechniCalc.ofFloat(1e12)))
  ).toBe("1,000,000");
});
