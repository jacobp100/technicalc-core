import * as TechniCalc from "../Value";
import { testPrefixes, testUnits } from "../ValueTestUtil";
import { convert } from "../Units/Units";

const c = (value, fromUnits, toUnits) =>
  TechniCalc.toFloat(convert(value, fromUnits, toUnits));

test("converts simple units", () => {
  expect(
    c(
      TechniCalc.one,
      [{ prefix: testPrefixes.unit, unit: testUnits.meter, power: 1 }],
      [{ prefix: testPrefixes.unit, unit: testUnits.inch, power: 1 }]
    )
  ).toBeCloseTo(39.37);
  expect(
    c(
      TechniCalc.one,
      [{ prefix: testPrefixes.unit, unit: testUnits.inch, power: 1 }],
      [{ prefix: testPrefixes.unit, unit: testUnits.meter, power: 1 }]
    )
  ).toBeCloseTo(0.0254);
  expect(
    c(
      TechniCalc.ofFloat(2),
      [{ prefix: testPrefixes.unit, unit: testUnits.meter, power: 1 }],
      [{ prefix: testPrefixes.unit, unit: testUnits.inch, power: 1 }]
    )
  ).toBeCloseTo(78.74);
});

test("converts exponentiated units", () => {
  expect(
    c(
      TechniCalc.one,
      [{ prefix: testPrefixes.unit, unit: testUnits.meter, power: 2 }],
      [{ prefix: testPrefixes.unit, unit: testUnits.inch, power: 2 }]
    )
  ).toBeCloseTo(1550);
  expect(
    c(
      TechniCalc.one,
      [{ prefix: testPrefixes.unit, unit: testUnits.inch, power: 2 }],
      [{ prefix: testPrefixes.unit, unit: testUnits.meter, power: 2 }]
    )
  ).toBeCloseTo(6.452e-4);
});

test("converts exponentiated units with prefixes", () => {
  expect(
    c(
      TechniCalc.one,
      [{ prefix: testPrefixes.milli, unit: testUnits.meter, power: 2 }],
      [{ prefix: testPrefixes.unit, unit: testUnits.meter, power: 2 }]
    )
  ).toBeCloseTo(1e-6);
  expect(
    c(
      TechniCalc.one,
      [{ prefix: testPrefixes.kilo, unit: testUnits.meter, power: 2 }],
      [{ prefix: testPrefixes.unit, unit: testUnits.meter, power: 2 }]
    )
  ).toBeCloseTo(1e6);
});

test("converts between different dimensions", () => {
  expect(
    c(
      TechniCalc.one,
      [{ prefix: testPrefixes.unit, unit: testUnits.acre, power: 1 }],
      [{ prefix: testPrefixes.unit, unit: testUnits.meter, power: 2 }]
    )
  ).toBeCloseTo(4047);
  expect(
    c(
      TechniCalc.one,
      [{ prefix: testPrefixes.unit, unit: testUnits.liter, power: 1 }],
      [{ prefix: testPrefixes.unit, unit: testUnits.meter, power: 3 }]
    )
  ).toBeCloseTo(0.001);
});
