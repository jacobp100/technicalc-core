import {
  one,
  minusOne,
  ofVector,
  add,
  div,
  mul,
  pi,
  i,
  nan,
} from "../Value/Value";
import {
  defaultFormat,
  ofString,
  toString as toStringBase,
} from "../Formatting/Formatting";
import { toString, matrixOfFloats } from "../__test-util__/ValueTestUtil";

const [three, minusThree, threeHalves, minusThreeHalves, half, minusHalf] = [
  "3",
  "-3",
  "1.5",
  "-1.5",
  "0.5",
  "-0.5",
].map(ofString);

test("parses decimal strings", () => {
  expect(ofString("0.5")).toEqual(half);
  expect(ofString("-0.5")).toEqual(minusHalf);
  expect(ofString(".5")).toEqual(half);
  expect(ofString("-.5")).toEqual(minusHalf);
});

test("formats large decimals", () => {
  expect(
    toStringBase(
      { ...defaultFormat, constants: false, precision: 99 },
      false,
      div(one, pi)
    )
  ).toEqual("0.318309886184");
});

it.each([
  [one, "1"],
  [minusOne, "-1"],
  [half, "1/2"],
  [minusHalf, "-1/2"],
  [i, "i"],
  [mul(minusOne, i), "-i"],
  [mul(half, i), "1/2i"],
  [mul(minusHalf, i), "-1/2i"],
  [add(one, i), "1+i"],
  [add(minusOne, i), "-1+i"],
  [add(one, mul(minusOne, i)), "1-i"],
  [add(minusOne, mul(minusOne, i)), "-1-i"],
  [mul(one, pi), "pi"],
  [mul(minusOne, pi), "-pi"],
  [mul(half, pi), "pi/2"],
  [mul(minusHalf, pi), "-pi/2"],
  [mul(three, pi), "3pi"],
  [mul(minusThree, pi), "-3pi"],
  [mul(threeHalves, pi), "3pi/2"],
  [mul(minusThreeHalves, pi), "-3pi/2"],
  [ofVector([one, one]), "{1,1}"],
  [ofVector([add(one, i), add(minusOne, i)]), "{1+i,-1+i}"],
  [matrixOfFloats(2, 2, [1, 0.5, -0.5, -1]), "{{1,1/2},{-1/2,-1}}"],
  [nan, "NaN"],
])("Formats %s to %s", (technicalcValue, formatted) => {
  expect(
    toString(technicalcValue, {
      ...defaultFormat,
      fractions: "improper",
    })
  ).toBe(formatted);
  expect(ofString(formatted)).toEqual(technicalcValue);
});

it.each(["123", "456", "-123", "-456", "5sqrt(7)", "8exp(8)", "8e14"])(
  "Formats back and forth between %s",
  (value) => {
    expect(toString(ofString(value))).toBe(value);
  }
);

const format = (value, mode) =>
  toString(value, mode != null ? { mode } : undefined);

test("formats Tex", () => {
  expect(format(one, "tex")).toBe("1");
  expect(format(minusOne, "tex")).toBe("-1");
  expect(format(half, "tex", { fractions: "improper" })).toBe("\\frac{1}{2}");
  expect(format(minusHalf, "tex")).toBe("-\\frac{1}{2}");
  expect(format(add(one, i), "tex")).toBe("1+i");
  expect(format(add(minusOne, i), "tex")).toBe("-1+i");
  expect(format(add(one, mul(minusOne, i)), "tex")).toBe("1-i");
  expect(format(add(minusOne, mul(minusOne, i)), "tex")).toBe("-1-i");
  expect(format(mul(one, pi), "tex")).toBe("\\pi");
  expect(format(mul(minusOne, pi), "tex")).toBe("-\\pi");
  expect(format(mul(half, pi), "tex")).toBe("\\frac{\\pi}{2}");
  expect(format(mul(minusHalf, pi), "tex")).toBe("-\\frac{\\pi}{2}");
  expect(format(mul(three, pi), "tex")).toBe("3\\pi");
  expect(format(mul(minusThree, pi), "tex")).toBe("-3\\pi");
  expect(format(mul(threeHalves, pi), "tex")).toBe("\\frac{3\\pi}{2}");
  expect(format(mul(minusThreeHalves, pi), "tex")).toBe("-\\frac{3\\pi}{2}");
});

test("formats mathml", () => {
  const math = (x) =>
    `<math xmlns="http://www.w3.org/1998/Math/MathML" display="block">${x}</math>`;
  expect(format(one, "mathml")).toBe(math("<mn>1</mn>"));
  expect(format(minusOne, "mathml")).toBe(math("<mo>-</mo><mn>1</mn>"));
  expect(format(half, "mathml")).toBe(
    math("<mfrac><mn>1</mn><mn>2</mn></mfrac>")
  );
  expect(format(minusHalf, "mathml")).toBe(
    math("<mo>-</mo><mfrac><mn>1</mn><mn>2</mn></mfrac>")
  );
  expect(format(add(one, i), "mathml")).toBe(
    math("<mn>1</mn><mo>+</mo><mi>i</mi>")
  );
  expect(format(add(minusOne, i), "mathml")).toBe(
    math("<mo>-</mo><mn>1</mn><mo>+</mo><mi>i</mi>")
  );
  expect(format(add(one, mul(minusOne, i)), "mathml")).toBe(
    math("<mn>1</mn><mo>-</mo><mi>i</mi>")
  );
  expect(format(add(minusOne, mul(minusOne, i)), "mathml")).toBe(
    math("<mo>-</mo><mn>1</mn><mo>-</mo><mi>i</mi>")
  );
  expect(format(mul(one, pi), "mathml")).toBe(math("<mi>&#960;</mi>"));
  expect(format(mul(minusOne, pi), "mathml")).toBe(
    math("<mo>-</mo><mi>&#960;</mi>")
  );
  expect(format(mul(half, pi), "mathml")).toBe(
    math("<mfrac><mi>&#960;</mi><mn>2</mn></mfrac>")
  );
  expect(format(mul(minusHalf, pi), "mathml")).toBe(
    math("<mo>-</mo><mfrac><mi>&#960;</mi><mn>2</mn></mfrac>")
  );
  expect(format(mul(three, pi), "mathml")).toBe(
    math("<mn>3</mn><mi>&#960;</mi>")
  );
  expect(format(mul(minusThree, pi), "mathml")).toBe(
    math("<mo>-</mo><mn>3</mn><mi>&#960;</mi>")
  );
  expect(format(mul(threeHalves, pi), "mathml")).toBe(
    math("<mfrac><mrow><mn>3</mn><mi>&#960;</mi></mrow><mn>2</mn></mfrac>")
  );
  expect(format(mul(minusThreeHalves, pi), "mathml")).toBe(
    math(
      "<mo>-</mo><mfrac><mrow><mn>3</mn><mi>&#960;</mi></mrow><mn>2</mn></mfrac>"
    )
  );

  expect(format(one, "mathml-inline")).toBe(
    '<math xmlns="http://www.w3.org/1998/Math/MathML" display="inline"><mn>1</mn></math>'
  );
});
