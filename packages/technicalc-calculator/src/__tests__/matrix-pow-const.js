const { range } = require("lodash");
const mathjs = require("mathjs");
const TechniCalc = require("../Value.bs");
const { matrixOfFloats, toString } = require("../ValueTestUtil.bs");
const { Value, toMatchJsMatrix } = require("../__test-util__");
const { matrix3x3 } = require("../__test-util__/math-js-matrix");
const cartesian = require("../__test-util__/cartesian");

expect.extend({ toMatchJsMatrix });

describe("3x3", () => {
  cartesian([matrix3x3, range(0, 2 + 1).map(Value.float)]).forEach(([a, b]) => {
    it(`${a.title} ** ${b.title}`, () => {
      const mathJsValue = mathjs.pow(a.jsValue, b.jsValue);
      expect(
        TechniCalc.pow(a.techniCalcValue, b.techniCalcValue)
      ).toMatchJsMatrix(mathJsValue);
    });
  });
});

describe("Pow -1", () => {
  // Checked on Wolfram Alpha
  it("2x2 ** -1", () => {
    const out = toString(
      TechniCalc.pow(matrixOfFloats(2, 2, [3, 7, 8, 9]), TechniCalc.ofFloat(-1))
    );
    expect(out).toBe("{{-9/29,7/29},{8/29,-3/29}}");
  });

  it("3x3 ** -1", () => {
    const out = toString(
      TechniCalc.pow(
        matrixOfFloats(3, 3, [3, 7, 8, 9, 1, 3, 9, 5, 8]),
        TechniCalc.ofFloat(-1)
      )
    );
    expect(out).toBe("{{7/48,1/3,-13/48},{15/16,1,-21/16},{-3/4,-1,5/4}}");
  });
});
