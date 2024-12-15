import { ofString, ofStringBase } from "../Formatting/Formatting";
import * as TechniCalcTest from "../__test-util__/ValueTestUtil";

test("parses strings", () => {
  const convert = (x, opt) => TechniCalcTest.toString(ofString(x), opt);

  expect(convert("1")).toBe("1");
  expect(convert("100")).toBe("100");
  expect(convert("1e2")).toBe("100");
  expect(convert("1e+2")).toBe("100");
  expect(convert("1e-2")).toBe("1/100");
  expect(convert("1e3")).toBe("1,000");
  expect(convert("1e6")).toBe("1,000,000");
  expect(convert("1e9", { decimalMaxMagnitude: 12 })).toBe("1,000,000,000");
  expect(convert("1e1000")).toBe("1e1000");
  expect(convert("1.23456789", { fractions: "never" })).toBe("1.23456789");
  expect(convert("1.23456789e-100")).toBe("1.23456789e-100");
  expect(convert("1.23456789e100")).toBe("1.23456789e100");
  expect(convert("-1")).toBe("-1");
  expect(convert("-1e2")).toBe("-100");
  expect(convert("-1e+2")).toBe("-100");
  expect(convert("-1e-2")).toBe("-1/100");
  expect(convert("-1e1000")).toBe("-1e1000");
  expect(convert("-1.23456789", { fractions: "never" })).toBe("-1.23456789");
  expect(convert("-1.23456789e-100")).toBe("-1.23456789e-100");
  expect(convert("-1.23456789e100")).toBe("-1.23456789e100");
});

test("parses strings in other bases", () => {
  const convert = (x, base, opt) =>
    TechniCalcTest.toString(ofStringBase(base, x), opt);

  expect(convert("1", 16)).toBe("1");
  expect(convert("100", 16)).toBe("256");
  expect(convert("FF", 16)).toBe("255");
  expect(convert("E", 16)).toBe("14");
  expect(convert("BEAD", 16)).toBe("48,813");
  expect(
    convert("1.1", 2, {
      fractions: "improper",
    })
  ).toBe("3/2");
});

test("converts decimals to fractions", () => {
  expect(
    TechniCalcTest.toString(ofString("0.4"), { fractions: "improper" })
  ).toBe("2/5");

  expect(
    TechniCalcTest.toString(ofString("0.123456789"), {
      fractions: "never",
      maxDecimalPlaces: 12,
    })
  ).toBe("0.123456789");
});
