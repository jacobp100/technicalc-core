const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { trigValues } = require("../__test-util__/math-js");
const TechniCalc = require("../Value.bs");

expect.extend({ toMatchJsValue });

test.each(trigValues)("tan(%s)", (v) => {
  const actual = TechniCalc.tan(v.techniCalcValue);
  const expected = mathjs.tan(v.jsValue);
  expect(actual).toMatchJsValue(expected);
});
