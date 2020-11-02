const { range } = require("lodash");
const mathjs = require("mathjs");
const TechniCalc = require("../Value.bs");
const { Value, toMatchJsValue, toMatchJsMatrix } = require("../__test-util__");
const { matrix2x2, matrix3x3 } = require("../__test-util__/math-js-matrix");
const cartesian = require("../__test-util__/cartesian");

expect.extend({ toMatchJsValue, toMatchJsMatrix });

const constantValues = range(-2, 2 + 1).map(Value.float);
const values = [
  ...cartesian([matrix2x2, constantValues]),
  ...cartesian([matrix3x3, constantValues]),
];

test("mul", () => {
  values.forEach(([a, b]) => {
    const actualValue = TechniCalc.mul(a.techniCalcValue, b.techniCalcValue);
    const expectedValue = mathjs.multiply(a.jsValue, b.jsValue);
    expect(actualValue).toMatchJsMatrix(expectedValue, () => `${a} * ${b}`);
  });
});

test("div", () => {
  values.forEach(([a, b]) => {
    const actualValue = TechniCalc.div(a.techniCalcValue, b.techniCalcValue);

    if (b.jsValue !== 0) {
      const expectedValue = mathjs.divide(a.jsValue, b.jsValue);
      expect(actualValue).toMatchJsMatrix(expectedValue, () => `${a} / ${b}`);
    } else {
      expect(actualValue).toMatchJsValue(NaN);
    }
  });
});
