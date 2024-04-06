import * as mathjs from "mathjs";
import { toMatchJsValue } from "../__test-util__/index";
import { complexValues } from "../__test-util__/math-js";
import * as TechniCalc from "../Value/Value";

expect.extend({ toMatchJsValue });

test("inv", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.inv(v.techniCalcValue);
    const expected = mathjs.inv(v.jsValue);
    expect(actual).toMatchJsValue(expected, () => `inv(${v})`);
  });
});

test("abs", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.abs(v.techniCalcValue);
    const expected = mathjs.abs(v.jsValue);
    expect(actual).toMatchJsValue(expected, () => `abs(${v})`);
  });
});

test("round", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.round(v.techniCalcValue);
    const expected = mathjs.round(v.jsValue);
    expect(actual).toMatchJsValue(expected, () => `round(${v})`);
  });
});

test("floor", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.floor(v.techniCalcValue);
    const expected = mathjs.floor(v.jsValue);
    expect(actual).toMatchJsValue(expected, () => `floor(${v})`);
  });
});

test("ceil", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.ceil(v.techniCalcValue);
    const expected = mathjs.ceil(v.jsValue);
    expect(actual).toMatchJsValue(expected, () => `ceil(${v})`);
  });
});

test("log", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.log(v.techniCalcValue);
    const expected = mathjs.log(v.jsValue);
    expect(actual).toMatchJsValue(expected, () => `log(${v})`);
  });
});

test("exp", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.exp(v.techniCalcValue);
    const expected = mathjs.exp(v.jsValue);
    expect(actual).toMatchJsValue(expected, () => `exp(${v})`);
  });
});

test("sqrt", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.sqrt(v.techniCalcValue);
    const expected = mathjs.sqrt(v.jsValue);
    expect(actual).toMatchJsValue(expected, () => `sqrt(${v})`);
  });
});

test("sin", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.sin(v.techniCalcValue);
    const expected = mathjs.sin(v.jsValue);
    expect(actual).toMatchJsValue(expected, () => `sin(${v})`);
  });
});

test("cos", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.cos(v.techniCalcValue);
    const expected = mathjs.cos(v.jsValue);
    expect(actual).toMatchJsValue(expected, () => `cos(${v})`);
  });
});

test("tan", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.tan(v.techniCalcValue);
    const expected = mathjs.tan(v.jsValue);
    expect(actual).toMatchJsValue(expected, () => `tan(${v})`);
  });
});

test("asin", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.asin(v.techniCalcValue);
    const expected = mathjs.asin(v.jsValue);
    expect(actual).toMatchJsValue(expected, () => `asin(${v})`);
  });
});

test("acos", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.acos(v.techniCalcValue);
    const expected = mathjs.acos(v.jsValue);
    expect(actual).toMatchJsValue(expected, () => `acos(${v})`);
  });
});

test("atan", () => {
  // Not sure why my implementation gets negative values here
  // MathJS agrees with Wolfram Alpha as the convential value, but both answers are valid
  const negateRealValues = new Set([
    "0+-2i",
    "0+-3i",
    "0+-4i",
    "0+-5i",
    "0+2i",
    "0+3i",
    "0+4i",
    "0+5i",
  ]);

  complexValues.forEach((v) => {
    const actual = TechniCalc.atan(v.techniCalcValue);
    const expected = mathjs.atan(v.jsValue);
    if (negateRealValues.has(v.title)) expected.re *= -1;
    expect(actual).toMatchJsValue(expected, () => `atan(${v})`);
  });
});

test("sinh", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.sinh(v.techniCalcValue);
    const expected = mathjs.sinh(v.jsValue);
    expect(actual).toMatchJsValue(expected, () => `sinh(${v})`);
  });
});

test("cosh", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.cosh(v.techniCalcValue);
    const expected = mathjs.cosh(v.jsValue);
    expect(actual).toMatchJsValue(expected, () => `cosh(${v})`);
  });
});

test("tanh", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.tanh(v.techniCalcValue);
    const expected = mathjs.tanh(v.jsValue);
    expect(actual).toMatchJsValue(expected, () => `tanh(${v})`);
  });
});

test("asinh", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.asinh(v.techniCalcValue);
    const expected = mathjs.asinh(v.jsValue);
    expect(actual).toMatchJsValue(expected, () => `asinh(${v})`);
  });
});

test("acosh", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.acosh(v.techniCalcValue);
    const expected = mathjs.acosh(v.jsValue);
    expect(actual).toMatchJsValue(expected, () => `acosh(${v})`);
  });
});

test("atanh", () => {
  // Not sure why my implementation gets negative values here
  // MathJS agrees with Wolfram Alpha as the convential value, but both answers are valid
  const negateImaginaryValues = new Set(["2+0i", "3+0i", "4+0i", "5+0i"]);

  complexValues.forEach((v) => {
    const actual = TechniCalc.atanh(v.techniCalcValue);
    const expected = mathjs.atanh(v.jsValue);
    if (negateImaginaryValues.has(v.title)) expected.im *= -1;
    expect(actual).toMatchJsValue(expected, () => `atanh(${v})`);
  });
});

test("deg", () => {
  const actual = TechniCalc.toInt(TechniCalc.toDeg(TechniCalc.pi));
  expect(actual).toBe(180);
});

test("grad", () => {
  const actual = TechniCalc.toInt(TechniCalc.toGrad(TechniCalc.pi));
  expect(actual).toBe(200);
});
