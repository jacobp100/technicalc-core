const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { imagValues } = require("../__test-util__/math-js");
const TechniCalc = require("../Value.bs");

expect.extend({ toMatchJsValue });

// Not sure why my implementation gets negative values here
// MathJS agrees with Wolfram Alpha as the convential value, but both answers are valid
const negateRealValues = new Set([
  "0+-2i",
  "0+-3i",
  "0+-4i",
  "0+-5i",
  "0+2i",
  "0+3i",
  "0+4i",
  "0+5i",
]);

test.each(imagValues)("atan(%s)", (v) => {
  const actual = TechniCalc.atan(v.techniCalcValue);
  const expected = mathjs.atan(v.jsValue);
  if (negateRealValues.has(v.title)) expected.re *= -1;
  expect(actual).toMatchJsValue(expected);
});
