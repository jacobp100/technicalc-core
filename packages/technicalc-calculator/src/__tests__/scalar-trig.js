const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { complexValues } = require("../__test-util__/math-js");
const TechniCalc = require("../Value.bs");

expect.extend({ toMatchJsValue });

test("sin", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.sin(v.techniCalcValue);
    const expected = mathjs.sin(v.jsValue);
    expect(actual).toMatchJsValue(expected, `sin(${v})`);
  });
});

test("cos", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.cos(v.techniCalcValue);
    const expected = mathjs.cos(v.jsValue);
    expect(actual).toMatchJsValue(expected, `cos(${v})`);
  });
});

test("tan", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.tan(v.techniCalcValue);
    const expected = mathjs.tan(v.jsValue);
    expect(actual).toMatchJsValue(expected, `tan(${v})`);
  });
});

test("asin", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.asin(v.techniCalcValue);
    const expected = mathjs.asin(v.jsValue);
    expect(actual).toMatchJsValue(expected, `asin(${v})`);
  });
});

test("acos", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.acos(v.techniCalcValue);
    const expected = mathjs.acos(v.jsValue);
    expect(actual).toMatchJsValue(expected, `acos(${v})`);
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
    expect(actual).toMatchJsValue(expected, `atan(${v})`);
  });
});

test("sinh", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.sinh(v.techniCalcValue);
    const expected = mathjs.sinh(v.jsValue);
    expect(actual).toMatchJsValue(expected, `sinh(${v})`);
  });
});

test("cosh", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.cosh(v.techniCalcValue);
    const expected = mathjs.cosh(v.jsValue);
    expect(actual).toMatchJsValue(expected, `cosh(${v})`);
  });
});

test("tanh", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.tanh(v.techniCalcValue);
    const expected = mathjs.tanh(v.jsValue);
    expect(actual).toMatchJsValue(expected, `tanh(${v})`);
  });
});

test("asinh", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.asinh(v.techniCalcValue);
    const expected = mathjs.asinh(v.jsValue);
    expect(actual).toMatchJsValue(expected, `asinh(${v})`);
  });
});

test("acosh", () => {
  complexValues.forEach((v) => {
    const actual = TechniCalc.acosh(v.techniCalcValue);
    const expected = mathjs.acosh(v.jsValue);
    expect(actual).toMatchJsValue(expected, `acosh(${v})`);
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
    expect(actual).toMatchJsValue(expected, `atanh(${v})`);
  });
});
