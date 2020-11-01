const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { imagValues } = require("../__test-util__/math-js");
const TechniCalc = require("../Value.bs");

expect.extend({ toMatchJsValue });

test.each(imagValues)("cosh(%s)", (v) => {
  const actual = TechniCalc.cosh(v.techniCalcValue);
  const expected = mathjs.cosh(v.jsValue);
  expect(actual).toMatchJsValue(expected);
});
