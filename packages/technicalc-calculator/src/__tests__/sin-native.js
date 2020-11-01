const { toMatchJsValue } = require("../__test-util__");
const { trigValues } = require("../__test-util__/math-native");
const TechniCalc = require("../Value.bs");

expect.extend({ toMatchJsValue });

test.each(trigValues)("sin(%s)", (v) => {
  expect(TechniCalc.sin(v.techniCalcValue)).toMatchJsValue(Math.sin(v.jsValue));
});
