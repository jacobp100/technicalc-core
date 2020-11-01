const TechniCalc = require("../Value.bs");
const { testUnits } = require("../ValueTestUtil.bs");
const { convert } = require("../Units/Units.bs");

const c = (value, fromUnits, toUnits) =>
  TechniCalc.toFloat(convert(value, fromUnits, toUnits));

test("converts simple units", () => {
  expect(
    c(TechniCalc.one, [[testUnits.meter, 1]], [[testUnits.inch, 1]])
  ).toBeCloseTo(39.37);
  expect(
    c(TechniCalc.one, [[testUnits.inch, 1]], [[testUnits.meter, 1]])
  ).toBeCloseTo(0.0254);
  expect(
    c(TechniCalc.ofFloat(2), [[testUnits.meter, 1]], [[testUnits.inch, 1]])
  ).toBeCloseTo(78.74);
});

test("converts exponentiated units", () => {
  expect(
    c(TechniCalc.one, [[testUnits.meter, 2]], [[testUnits.inch, 2]])
  ).toBeCloseTo(1550);
  expect(
    c(TechniCalc.one, [[testUnits.inch, 2]], [[testUnits.meter, 2]])
  ).toBeCloseTo(6.452e-4);
});

test("converts between different dimensions", () => {
  expect(
    c(TechniCalc.one, [[testUnits.acre, 1]], [[testUnits.meter, 2]])
  ).toBeCloseTo(4047);
  expect(
    c(TechniCalc.one, [[testUnits.liter, 1]], [[testUnits.meter, 3]])
  ).toBeCloseTo(0.001);
});
