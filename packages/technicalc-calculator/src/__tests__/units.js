import * as TechniCalc from "../Value";
import { testPrefixes, testUnits } from "../ValueTestUtil";
import { convert, convertComposite } from "../Units/Units";

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

test("converts temperatures", () => {
  expect(
    c(
      TechniCalc.ofFloat(50),
      [{ prefix: testPrefixes.unit, unit: testUnits.celsius, power: 1 }],
      [{ prefix: testPrefixes.unit, unit: testUnits.fahrenheit, power: 1 }]
    )
  ).toBeCloseTo(122);

  expect(
    c(
      TechniCalc.ofFloat(50),
      [{ prefix: testPrefixes.unit, unit: testUnits.fahrenheit, power: 1 }],
      [{ prefix: testPrefixes.unit, unit: testUnits.celsius, power: 1 }]
    )
  ).toBeCloseTo(10);
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

test("compound units", () => {
  expect(
    c(
      TechniCalc.one,
      [
        { prefix: testPrefixes.unit, unit: testUnits.meter, power: 1 },
        { prefix: testPrefixes.unit, unit: testUnits.second, power: -2 },
      ],
      [
        { prefix: testPrefixes.unit, unit: testUnits.inch, power: 1 },
        { prefix: testPrefixes.unit, unit: testUnits.hour, power: -2 },
      ]
    )
  ).toBeCloseTo(510236220.47244096);
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

test("incompatible units", () => {
  expect(
    c(
      TechniCalc.ofFloat(50),
      [{ prefix: testPrefixes.unit, unit: testUnits.celsius, power: 1 }],
      [{ prefix: testPrefixes.unit, unit: testUnits.second, power: 1 }]
    )
  ).toBe(NaN);
});

test("composite", () => {
  const [foot, inch] = convertComposite(
    [
      [
        TechniCalc.ofFloat(1),
        { prefix: testPrefixes.unit, unit: testUnits.meter, power: 1 },
      ],
    ],
    [
      { prefix: testPrefixes.unit, unit: testUnits.foot, power: 1 },
      { prefix: testPrefixes.unit, unit: testUnits.inch, power: 1 },
    ]
  );

  expect(TechniCalc.toFloat(foot)).toBe(3);
  expect(TechniCalc.toFloat(inch)).toBeCloseTo(3.37);
});

test("composite with negative units", () => {
  const [foot, inch] = convertComposite(
    [
      [
        TechniCalc.ofFloat(-1),
        { prefix: testPrefixes.unit, unit: testUnits.meter, power: 1 },
      ],
    ],
    [
      { prefix: testPrefixes.unit, unit: testUnits.foot, power: 1 },
      { prefix: testPrefixes.unit, unit: testUnits.inch, power: 1 },
    ]
  );

  expect(TechniCalc.toFloat(foot)).toBe(-3);
  expect(TechniCalc.toFloat(inch)).toBeCloseTo(-3.37);
});

test("composite with incompatible units", () => {
  const result = convertComposite(
    [
      [
        TechniCalc.ofFloat(1),
        { prefix: testPrefixes.unit, unit: testUnits.meter, power: 1 },
      ],
    ],
    [
      { prefix: testPrefixes.unit, unit: testUnits.foot, power: 1 },
      { prefix: testPrefixes.unit, unit: testUnits.second, power: 1 },
    ]
  );

  expect(result).toBe(undefined);
});
