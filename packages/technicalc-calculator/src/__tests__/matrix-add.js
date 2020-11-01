const mathjs = require("mathjs");
const TechniCalc = require("../Value.bs");
const TechniCalcTest = require("../ValueTestUtil.bs");
const { toMatchJsMatrix } = require("../__test-util__");
const { matrix2x2, matrix3x3 } = require("../__test-util__/math-js-matrix");
const cartesian = require("../__test-util__/cartesian");

expect.extend({ toMatchJsMatrix });

describe("2x2", () => {
  test.each(cartesian([matrix2x2, matrix2x2]))("%s + %s", (a, b) => {
    const actualValue = TechniCalc.add(a.techniCalcValue, b.techniCalcValue);
    const expectedValue = mathjs.add(a.jsValue, b.jsValue);
    expect(actualValue).toMatchJsMatrix(expectedValue);
  });
});

describe("3x3", () => {
  test.each(cartesian([matrix3x3, matrix3x3]))("%s + %s", (a, b) => {
    const actualValue = TechniCalc.add(a.techniCalcValue, b.techniCalcValue);
    const expectedValue = mathjs.add(a.jsValue, b.jsValue);
    expect(actualValue).toMatchJsMatrix(expectedValue);
  });
});

describe("Type checking", () => {
  it("Cannot add incompatible sizes", () => {
    const value = TechniCalc.add(
      matrix2x2[0].techniCalcValue,
      matrix3x3[0].techniCalcValue
    );
    expect(TechniCalcTest.toString(value)).toBe("NaN");
  });
});
