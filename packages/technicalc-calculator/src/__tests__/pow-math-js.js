const { range } = require("lodash");
const mathjs = require("mathjs");
const TechniCalc = require("../Value.bs");
const { Value, toMatchJsValue } = require("../__test-util__");
const cartesian = require("../__test-util__/cartesian");

expect.extend({ toMatchJsValue });

const mathematicallyAccuratePow = (a, b) => {
  // MathJS seems to mess up when a == 0
  if (mathjs.equal(a, 0)) return mathjs.equal(b, 0) ? NaN : 0;
  if (mathjs.equal(b, 0)) return 1;
  return mathjs.pow(a, b);
};

const expValues = [].concat(
  cartesian([range(-2, 2 + 2), range(-2, 2 + 2)]).map(([re, im]) =>
    Value.complex(re, im)
  ),
  Value.e(1),
  Value.e(2),
  Value.pi(1),
  Value.pi(2)
);

const values = cartesian([expValues, expValues]);

test.each(values)("(%s) ** (%s)", (a, b) => {
  const actual = TechniCalc.pow(a.techniCalcValue, b.techniCalcValue);
  const expected = mathematicallyAccuratePow(a.jsValue, b.jsValue);
  expect(actual).toMatchJsValue(expected);
});
