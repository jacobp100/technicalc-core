const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { imagValues } = require("../__test-util__/math-js");
const TechniCalc = require("../Value.bs");

expect.extend({ toMatchJsValue });

test.each(imagValues)("asinh(%s)", (v) => {
  const actual = TechniCalc.asinh(v.techniCalcValue);
  const expected = mathjs.asinh(v.jsValue);
  expect(actual).toMatchJsValue(expected);
});
