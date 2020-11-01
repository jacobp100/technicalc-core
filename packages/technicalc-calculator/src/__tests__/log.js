const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { imagValues } = require("../__test-util__/math-js");
const TechniCalc = require("../Value.bs");

expect.extend({ toMatchJsValue });

test.each(imagValues)("log(%s)", (v) => {
  const actual = TechniCalc.log(v.techniCalcValue);
  const expected = mathjs.log(v.jsValue);
  expect(actual).toMatchJsValue(expected);
});
