const { range } = require("lodash");
const mathjs = require("mathjs");
const TechniCalc = require("../Value.bs");
const { matrixOfFloats, toString } = require("../ValueTestUtil.bs");
const { Value, toMatchJsValue, toMatchJsMatrix } = require("../__test-util__");
const { matrix2x2, matrix3x3 } = require("../__test-util__/math-js-matrix");
const cartesian = require("../__test-util__/cartesian");

expect.extend({ toMatchJsValue, toMatchJsMatrix });

const values = [...matrix2x2, ...matrix3x3];

test("neg", () => {
  expect(TechniCalc.neg(TechniCalc.one)).toMatchJsValue(-1);

  expect(TechniCalc.neg(TechniCalc.ofFloat(Math.PI))).toMatchJsValue(-Math.PI);
});

test("pow", () => {
  cartesian([values, range(0, 2 + 1).map(Value.float)]).forEach(([a, b]) => {
    const mathJsValue = mathjs.pow(a.jsValue, b.jsValue);
    expect(
      TechniCalc.pow(a.techniCalcValue, b.techniCalcValue)
    ).toMatchJsMatrix(mathJsValue, `${a} ** ${b}`);
  });
});

test("det", () => {
  values.forEach((v) => {
    const actual = TechniCalc.abs(v.techniCalcValue);
    const expected = mathjs.det(v.jsValue);
    expect(actual).toMatchJsValue(expected, `det${v}`);
  });
});

test("inv", () => {
  const out2x2 = toString(
    TechniCalc.pow(matrixOfFloats(2, 2, [3, 7, 8, 9]), TechniCalc.ofFloat(-1))
  );
  expect(out2x2).toBe("{{-9/29,7/29},{8/29,-3/29}}");

  const out3x3 = toString(
    TechniCalc.pow(
      matrixOfFloats(3, 3, [3, 7, 8, 9, 1, 3, 9, 5, 8]),
      TechniCalc.ofFloat(-1)
    )
  );
  expect(out3x3).toBe("{{7/48,1/3,-13/48},{15/16,1,-21/16},{-3/4,-1,5/4}}");
});
