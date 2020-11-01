const TechniCalc = require("../Value.bs");
const TechniCalcTest = require("../ValueTestUtil.bs");

const stringOfFloat = (x, format) =>
  TechniCalcTest.toString(TechniCalc.ofFloat(x), format);

it("only adds commas for values greater than 999", () => {
  expect(stringOfFloat(100000000, { style: "decimal" })).toBe("100,000,000");
  expect(stringOfFloat(10000000, { style: "decimal" })).toBe("10,000,000");
  expect(stringOfFloat(1000000, { style: "decimal" })).toBe("1,000,000");
  expect(stringOfFloat(100000, { style: "decimal" })).toBe("100,000");
  expect(stringOfFloat(10000, { style: "decimal" })).toBe("10,000");
  expect(stringOfFloat(1000, { style: "decimal" })).toBe("1,000");
});

it("formats magnitude of reals", () => {
  expect(stringOfFloat(1000000000, { style: "decimal" })).toBe("1e9");
  expect(stringOfFloat(100, { style: "decimal" })).toBe("100");
  expect(stringOfFloat(10, { style: "decimal" })).toBe("10");
  expect(stringOfFloat(1, { style: "decimal" })).toBe("1");
  expect(stringOfFloat(0.1, { style: "decimal" })).toBe("0.1");
  expect(stringOfFloat(0.01, { style: "decimal" })).toBe("0.01");
  expect(stringOfFloat(0.001, { style: "decimal" })).toBe("0.001");
  expect(stringOfFloat(0.0001, { style: "decimal" })).toBe("1e-4");
});

it("trims trailling zeros", () => {
  expect(stringOfFloat(1.23e9, { style: "decimal" })).toBe("1.23e9");
  expect(stringOfFloat(1.23, { style: "decimal" })).toBe("1.23");
  expect(stringOfFloat(1.23e-4, { style: "decimal" })).toBe("1.23e-4");
  expect(stringOfFloat(1.234567890123456789e9, { style: "decimal" })).toBe(
    "1.234567890123e9"
  );
  expect(stringOfFloat(1.234567890123456789, { style: "decimal" })).toBe(
    "1.234567890123"
  );
  expect(stringOfFloat(1.234567890123456789e-4, { style: "decimal" })).toBe(
    "1.234567890123e-4"
  );
  expect(stringOfFloat(1.0000000000001, { style: "decimal" })).toBe("1");
});

it("formats scientific notation", () => {
  expect(stringOfFloat(1, { style: "scientific" })).toBe("1.000000000000e0");
  expect(stringOfFloat(10, { style: "scientific" })).toBe("10.000000000000e0");
  expect(stringOfFloat(100, { style: "scientific" })).toBe(
    "100.000000000000e0"
  );
  expect(stringOfFloat(1000, { style: "scientific" })).toBe("1.000000000000e3");
  expect(stringOfFloat(-1, { style: "scientific" })).toBe("-1.000000000000e0");
  expect(stringOfFloat(-10, { style: "scientific" })).toBe(
    "-10.000000000000e0"
  );
  expect(stringOfFloat(-100, { style: "scientific" })).toBe(
    "-100.000000000000e0"
  );
  expect(stringOfFloat(-1000, { style: "scientific" })).toBe(
    "-1.000000000000e3"
  );
});
