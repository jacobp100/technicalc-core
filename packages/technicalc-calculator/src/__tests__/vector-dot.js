const mathjs = require("mathjs");
const TechniCalc = require("../Value.bs");
const TechniCalcTest = require("../ValueTestUtil.bs");
const { toMatchJsValue } = require("../__test-util__");
const { vector2, vector3 } = require("../__test-util__/math-js-vector");
const cartesian = require("../__test-util__/cartesian");

expect.extend({ toMatchJsValue });

describe("2", () => {
  cartesian([vector2, vector2]).forEach(([a, b]) => {
    it(`${a.title} . ${b.title}`, () => {
      expect(
        TechniCalc.dot(a.techniCalcValue, b.techniCalcValue)
      ).toMatchJsValue(mathjs.dot(a.jsValue, b.jsValue));
    });
  });
});

describe("3", () => {
  cartesian([vector3, vector3]).forEach(([a, b]) => {
    it(`${a.title} . ${b.title}`, () => {
      expect(
        TechniCalc.dot(a.techniCalcValue, b.techniCalcValue)
      ).toMatchJsValue(mathjs.dot(a.jsValue, b.jsValue));
    });
  });
});

describe("Type checking", () => {
  it("Cannot dot incompatible sizes", () => {
    const value = TechniCalc.dot(
      vector3[0].techniCalcValue,
      vector2[0].techniCalcValue
    );
    expect(TechniCalcTest.toString(value)).toBe("NaN");
  });
});
