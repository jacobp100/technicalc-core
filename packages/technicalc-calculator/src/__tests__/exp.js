const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { imagValues } = require("../__test-util__/math-js");
const TechniCalc = require("../Value.bs");

expect.extend({ toMatchJsValue });

test.each(imagValues)("exp(%s)", (v) => {
  const actual = TechniCalc.exp(v.techniCalcValue);
  const expected = mathjs.exp(v.jsValue);
  expect(actual).toMatchJsValue(expected);
});
