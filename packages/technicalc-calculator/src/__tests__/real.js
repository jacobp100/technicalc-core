const {
  ofString,
  ofStringBase,
  ofFloat,
  pi,
  add,
  mul,
  div,
  sqrt,
  log,
  exp,
  sin,
} = require("../Value.bs");
const { toString } = require("../ValueTestUtil.bs");

test("Converts via ofString", () => {
  const convert = (x, opt) => toString(ofString(x), opt);
  expect(convert("1")).toBe("1");
  expect(convert("1e2")).toBe("100");
  expect(convert("1e+2")).toBe("100");
  expect(convert("1e-2")).toBe("1/100");
  expect(convert("1e3")).toBe("1,000");
  expect(convert("1e6")).toBe("1,000,000");
  expect(convert("1e9")).toBe("1,000,000,000");
  expect(convert("1e1000")).toBe("1e1000");
  expect(convert("1.23456789", { style: "decimal" })).toBe("1.23456789");
  expect(convert("1.23456789e-100")).toBe("1.23456789e-100");
  expect(convert("1.23456789e100")).toBe("1.23456789e100");
  expect(convert("-1")).toBe("-1");
  expect(convert("-1e2")).toBe("-100");
  expect(convert("-1e+2")).toBe("-100");
  expect(convert("-1e-2")).toBe("-1/100");
  expect(convert("-1e1000")).toBe("-1e1000");
  expect(convert("-1.23456789", { style: "decimal" })).toBe("-1.23456789");
  expect(convert("-1.23456789e-100")).toBe("-1.23456789e-100");
  expect(convert("-1.23456789e100")).toBe("-1.23456789e100");
});

test("Converts via ofStringBase", () => {
  const convert = (x, base, opt) => toString(ofStringBase(base, x), opt);
  expect(convert("1", 16)).toBe("1");
  expect(convert("FF", 16)).toBe("255");
  expect(convert("E", 16)).toBe("14");
  expect(convert("BEAD", 16)).toBe("48,813");
  expect(convert("1.1", 2)).toBe("3/2");
});

test("Converts decimals to fractions", () => {
  expect(toString(ofString("0.4"))).toBe("2/5");
  expect(toString(ofString("0.123456789"), { style: "decimal" })).toBe(
    "0.123456789"
  );
});

test("Simplifies division by two square roots", () => {
  expect(toString(div(sqrt(ofFloat(10)), sqrt(ofFloat(2))))).toEqual("sqrt(5)");
  expect(toString(div(sqrt(ofFloat(1000)), sqrt(ofFloat(2))))).toEqual(
    "10sqrt(5)"
  );
});

test("Tracks exp values through log", () => {
  expect(toString(log(exp(ofFloat(47))))).toEqual("47");
});

test("Simplifies square roots and exponentials", () => {
  expect(toString(mul(ofFloat(2), sqrt(ofFloat(2))))).toEqual("2sqrt(2)");
  expect(toString(sqrt(ofFloat(1000)))).toEqual("10sqrt(10)");
  expect(toString(sqrt(ofFloat(4)))).toEqual("2");
  expect(toString(sqrt(ofFloat(8)))).toEqual("2sqrt(2)");
  expect(toString(sqrt(ofFloat(6)))).toEqual("sqrt(6)");
  expect(toString(sqrt(ofFloat(12)))).toEqual("2sqrt(3)");
  expect(toString(sqrt(ofFloat(0)))).toEqual("0");
  expect(toString(exp(ofFloat(0)))).toEqual("1");
  expect(toString(exp(ofFloat(1)))).toEqual("exp(1)");
  expect(toString(exp(ofFloat(2)))).toEqual("exp(2)");
  expect(toString(exp(ofFloat(3)))).toEqual("exp(3)");
  expect(toString(exp(ofFloat(-1)))).toEqual("exp(-1)");
});

test("Does not simplify pi", () => {
  expect(toString(pi)).toEqual("pi");
});

test("Takes sin of pi + 1", () => {
  expect(toString(sin(add(pi, ofFloat(1))))).toBe("0.841470984807");
});

test("Formats various numbers correctly", () => {
  const convert = (x) => toString(ofString(x));
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
