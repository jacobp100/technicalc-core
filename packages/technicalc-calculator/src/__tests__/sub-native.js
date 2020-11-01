const { toMatchJsValue } = require("../__test-util__");
const { binaryValues } = require("../__test-util__/math-native");
const TechniCalc = require("../Value.bs");

expect.extend({ toMatchJsValue });

test.each(binaryValues)("%s - %s", (a, b) => {
  const actual = TechniCalc.sub(a.techniCalcValue, b.techniCalcValue);
  const expected = a.jsValue - b.jsValue;
  expect(actual).toMatchJsValue(expected);
});
