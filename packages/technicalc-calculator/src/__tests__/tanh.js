const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { imagValues } = require("../__test-util__/math-js");
const TechniCalc = require("../Value.bs");

expect.extend({ toMatchJsValue });

test.each(imagValues)(`tanh(%s)`, (v) => {
  const actual = TechniCalc.tanh(v.techniCalcValue);
  const expected = mathjs.tanh(v.jsValue);
  expect(actual).toMatchJsValue(expected);
});
