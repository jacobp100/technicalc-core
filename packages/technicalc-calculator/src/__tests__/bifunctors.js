const { toMatchJsValue } = require("../__test-util__");
const TechniCalc = require("../Value.bs");

expect.extend({ toMatchJsValue });

test("max", () => {
  expect(
    TechniCalc.max(TechniCalc.ofInt(3), TechniCalc.ofInt(5))
  ).toMatchJsValue(5);

  expect(
    TechniCalc.max(TechniCalc.ofInt(5), TechniCalc.ofInt(3))
  ).toMatchJsValue(5);
});

test("min", () => {
  expect(
    TechniCalc.min(TechniCalc.ofInt(3), TechniCalc.ofInt(5))
  ).toMatchJsValue(3);

  expect(
    TechniCalc.min(TechniCalc.ofInt(5), TechniCalc.ofInt(3))
  ).toMatchJsValue(3);
});

test("gcd", () => {
  expect(
    TechniCalc.gcd(TechniCalc.ofInt(20), TechniCalc.ofInt(25))
  ).toMatchJsValue(5);

  expect(
    TechniCalc.gcd(TechniCalc.ofInt(25), TechniCalc.ofInt(20))
  ).toMatchJsValue(5);
});

test("lcm", () => {
  expect(
    TechniCalc.lcm(TechniCalc.ofInt(20), TechniCalc.ofInt(25))
  ).toMatchJsValue(100);

  expect(
    TechniCalc.lcm(TechniCalc.ofInt(25), TechniCalc.ofInt(20))
  ).toMatchJsValue(100);
});
