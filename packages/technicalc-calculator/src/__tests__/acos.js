const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { imagValues } = require("../__test-util__/math-js");
const TechniCalc = require("../Value.bs");

expect.extend({ toMatchJsValue });

test.each(imagValues)("acos(%s)", (v) => {
  const actual = TechniCalc.acos(v.techniCalcValue);
  const expected = mathjs.acos(v.jsValue);
  expect(actual).toMatchJsValue(expected);
});
