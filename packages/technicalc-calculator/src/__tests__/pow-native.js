const { toMatchJsValue } = require("../__test-util__");
const { positiveBinaryValues } = require("../__test-util__/math-native");
const TechniCalc = require("../Value.bs");

expect.extend({ toMatchJsValue });

const mathematicallyAccuratePow = (a, b) => (a === 0 && b === 0 ? NaN : a ** b);

test.each(positiveBinaryValues)("(%s) ** (%s)", (a, b) => {
  const actual = TechniCalc.pow(a.techniCalcValue, b.techniCalcValue);
  const expected = mathematicallyAccuratePow(a.jsValue, b.jsValue);
  expect(actual).toMatchJsValue(expected);
});
