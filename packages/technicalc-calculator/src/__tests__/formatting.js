const TechniCalc = require("../Value.bs");
const TechniCalcTest = require("../ValueTestUtil.bs");

const stringOfFloat = (x, format) =>
  TechniCalcTest.toString(TechniCalc.ofFloat(x), format);

test("only adds commas for values greater than 999", () => {
  expect(stringOfFloat(100000000, { style: "decimal" })).toBe("100,000,000");
  expect(stringOfFloat(10000000, { style: "decimal" })).toBe("10,000,000");
  expect(stringOfFloat(1000000, { style: "decimal" })).toBe("1,000,000");
  expect(stringOfFloat(100000, { style: "decimal" })).toBe("100,000");
  expect(stringOfFloat(10000, { style: "decimal" })).toBe("10,000");
  expect(stringOfFloat(1000, { style: "decimal" })).toBe("1,000");
});

test("formats magnitude of reals", () => {
  expect(stringOfFloat(1000000000, { style: "decimal" })).toBe("1e9");
  expect(stringOfFloat(100, { style: "decimal" })).toBe("100");
  expect(stringOfFloat(10, { style: "decimal" })).toBe("10");
  expect(stringOfFloat(1, { style: "decimal" })).toBe("1");
  expect(stringOfFloat(0.1, { style: "decimal" })).toBe("0.1");
  expect(stringOfFloat(0.01, { style: "decimal" })).toBe("0.01");
  expect(stringOfFloat(0.001, { style: "decimal" })).toBe("0.001");
  expect(stringOfFloat(0.0001, { style: "decimal" })).toBe("1e-4");
});

test("trims trailling zeros", () => {
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

test("formats scientific notation", () => {
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

test("formats various numbers correctly", () => {
  const convert = (x) => TechniCalcTest.toString(TechniCalc.ofString(x));

  expect(convert("46.47897327055571")).toBe("46.478973270555");
  expect(convert("-47.86759243619015")).toBe("-47.86759243619");
  expect(convert("7.712346515387281")).toBe("7.712346515387");
  expect(convert("-41.08525582534328")).toBe("-41.085255825343");
  expect(convert("24.036159870635387")).toBe("24.036159870635");
  expect(convert("21.622267655248706")).toBe("21.622267655248");
  expect(convert("85.87032800784263")).toBe("85.870328007842");
  expect(convert("6.759552690635729")).toBe("6.759552690635");
  expect(convert("17.724509834485048")).toBe("17.724509834485");
  expect(convert("-17.618661853244163")).toBe("-17.618661853244");
  expect(convert("-71.09059285654436")).toBe("-71.090592856544");
  expect(convert("47.438865505981084")).toBe("47.438865505981");
  expect(convert("-18.28378337248739")).toBe("-18.283783372487");
  expect(convert("71.18764618368766")).toBe("71.187646183687");
  expect(convert("-66.96121712260108")).toBe("-66.961217122601");
  expect(convert("28.50749266445851")).toBe("28.507492664458");
  expect(convert("16.454703415645668")).toBe("16.454703415645");
  expect(convert("-64.43380990868866")).toBe("-64.433809908688");
  expect(convert("-66.90487607393479")).toBe("-66.904876073934");
  expect(convert("-28.212396089967342")).toBe("-28.212396089967");
});
