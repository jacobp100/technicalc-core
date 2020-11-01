const { range } = require("lodash");
const mathjs = require("mathjs");
const { Value, toMatchJsValue } = require("../__test-util__");
const cartesian = require("../__test-util__/cartesian");
const TechniCalc = require("../Value.bs");

expect.extend({ toMatchJsValue });

const baseValues = [...range(-2, 2 + 0.1, 0.1), ...range(2, 6 + 0.5, 0.5)];
const values = cartesian([baseValues, baseValues]).map(([re, im]) =>
  Value.complex(re, im)
);

const correctAnswers = new Map([
  [
    "-0.9999999999999992+6.38378239159465e-16i",
    { re: 7.637036285386e14, im: -6.094147220327e14 },
  ],
]);

test.each(values)("(%s)!", ({ title, techniCalcValue, jsValue }) => {
  const actual = TechniCalc.factorial(techniCalcValue);
  const expected =
    correctAnswers.get(title) ||
    mathjs.gamma(mathjs.complex(jsValue.re + 1, jsValue.im));
  expect(actual).toMatchJsValue(expected);
});
