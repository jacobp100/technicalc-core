import _ from "lodash";
import * as mathjs from "mathjs/lib/esm";
import * as TechniCalc from "../Value/Value";
import { Value, toMatchJsValue } from "../__test-util__/index";
import cartesian from "../__test-util__/cartesian";

expect.extend({ toMatchJsValue });

const baseValues = [
  ..._.range(-2, 2 + 0.1, 0.1),
  ..._.range(2, 6 + 0.5, 0.5),
].map((x) => Math.round(x * 10) / 10);

const realValues = baseValues.map((re) => Value.float(re));
const complexValues = cartesian([baseValues, baseValues]).map(([re, im]) =>
  Value.complex(re, im)
);

const correctAnswers = new Map([
  [
    "-0.9999999999999992+6.38378239159465e-16i",
    { re: 7.637036285386e14, im: -6.094147220327e14 },
  ],
]);

test("factorial 12", () => {
  expect(TechniCalc.factorial(TechniCalc.ofInt(12))).toMatchJsValue(479001600);
});

test("factorial real", () => {
  realValues.forEach((v) => {
    const { techniCalcValue, jsValue } = v;
    const actual = TechniCalc.factorial(techniCalcValue);
    const expected = mathjs.gamma(mathjs.complex(jsValue + 1));
    expect(actual).toMatchJsValue(expected, () => `(${v})!`);
  });
});

test("factorial complex", () => {
  complexValues.forEach((v) => {
    const { title, techniCalcValue, jsValue } = v;
    const actual = TechniCalc.factorial(techniCalcValue);
    const expected =
      correctAnswers.get(title) ||
      mathjs.gamma(mathjs.complex(jsValue.re + 1, jsValue.im));
    expect(actual).toMatchJsValue(expected, () => `(${v})!`);
  });
});
