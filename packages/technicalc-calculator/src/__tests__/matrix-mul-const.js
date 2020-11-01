const { range } = require("lodash");
const mathjs = require("mathjs");
const TechniCalc = require("../Value.bs");
const { Value, toMatchJsMatrix } = require("../__test-util__");
const { matrix2x2, matrix3x3 } = require("../__test-util__/math-js-matrix");
const cartesian = require("../__test-util__/cartesian");

expect.extend({ toMatchJsMatrix });

const constantValues = range(-2, 2 + 1).map(Value.float);

describe("2x2", () => {
  test.each(cartesian([matrix2x2, constantValues]))("%s * %s", (a, b) => {
    const actualValue = TechniCalc.mul(a.techniCalcValue, b.techniCalcValue);
    const expectedValue = mathjs.multiply(a.jsValue, b.jsValue);
    expect(actualValue).toMatchJsMatrix(expectedValue);
  });
});

describe("3x3", () => {
  test.each(cartesian([matrix3x3, constantValues]))("%s * %s", (a, b) => {
    const actualValue = TechniCalc.mul(a.techniCalcValue, b.techniCalcValue);
    const expectedValue = mathjs.multiply(a.jsValue, b.jsValue);
    expect(actualValue).toMatchJsMatrix(expectedValue);
  });
});
