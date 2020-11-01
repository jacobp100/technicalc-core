const { toMatchJsValue } = require("../__test-util__");
const { trigValues } = require("../__test-util__/math-native");
const TechniCalc = require("../Value.bs");

expect.extend({ toMatchJsValue });

test.each(trigValues)("cos(%s)", (v) => {
  expect(TechniCalc.cos(v.techniCalcValue)).toMatchJsValue(Math.cos(v.jsValue));
});
