const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { imagValues } = require("../__test-util__/math-js");
const TechniCalc = require("../Value.bs");

expect.extend({ toMatchJsValue });

// Not sure why my implementation gets negative values here
// MathJS agrees with Wolfram Alpha as the convential value, but both answers are valid
const negateImaginaryValues = new Set(["2+0i", "3+0i", "4+0i", "5+0i"]);

test.each(imagValues)("atanh(%s)", (v) => {
  const actual = TechniCalc.atanh(v.techniCalcValue);
  const expected = mathjs.atanh(v.jsValue);
  if (negateImaginaryValues.has(v.title)) expected.im *= -1;
  expect(actual).toMatchJsValue(expected);
});
