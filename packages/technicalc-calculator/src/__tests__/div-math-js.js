const mathjs = require("mathjs");
const TechniCalc = require("../Value.bs");
const { toMatchJsValue } = require("../__test-util__");
const { imagValues } = require("../__test-util__/math-js");
const cartesian = require("../__test-util__/cartesian");

expect.extend({ toMatchJsValue });

const values = cartesian([imagValues, imagValues]);

test.each(values)("%s / %s", (a, b) => {
  const actual = TechniCalc.div(a.techniCalcValue, b.techniCalcValue);
  const expected = mathjs.divide(a.jsValue, b.jsValue);
  expect(actual).toMatchJsValue(expected);
});
