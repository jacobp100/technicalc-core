open Jest
open Units

test("converts simple units", () => {
  expect(
    convert(
      Real.one,
      ~fromUnits=ofArray([{prefix: Unit, name: Meter, power: 1}]),
      ~toUnits=ofArray([{prefix: Unit, name: Inch, power: 1}]),
    )->Real.toFloat,
  )->toBeCloseTo(39.37)
  expect(
    convert(
      Real.one,
      ~fromUnits=ofArray([{prefix: Unit, name: Inch, power: 1}]),
      ~toUnits=ofArray([{prefix: Unit, name: Meter, power: 1}]),
    )->Real.toFloat,
  )->toBeCloseTo(0.0254)
  expect(
    convert(
      Real.ofInt(2),
      ~fromUnits=ofArray([{prefix: Unit, name: Meter, power: 1}]),
      ~toUnits=ofArray([{prefix: Unit, name: Inch, power: 1}]),
    )->Real.toFloat,
  )->toBeCloseTo(78.74)
})

test("converts temperatures", () => {
  expect(
    convert(
      Real.ofInt(50),
      ~fromUnits=ofArray([{prefix: Unit, name: Celsius, power: 1}]),
      ~toUnits=ofArray([{prefix: Unit, name: Fahrenheit, power: 1}]),
    )->Real.toFloat,
  )->toBeCloseTo(122.)

  expect(
    convert(
      Real.ofInt(50),
      ~fromUnits=ofArray([{prefix: Unit, name: Fahrenheit, power: 1}]),
      ~toUnits=ofArray([{prefix: Unit, name: Celsius, power: 1}]),
    )->Real.toFloat,
  )->toBeCloseTo(10.)
})

test("converts exponentiated units", () => {
  expect(
    convert(
      Real.one,
      ~fromUnits=ofArray([{prefix: Unit, name: Meter, power: 2}]),
      ~toUnits=ofArray([{prefix: Unit, name: Inch, power: 2}]),
    )->Real.toFloat,
  )->toBeCloseTo(1550.)
  expect(
    convert(
      Real.one,
      ~fromUnits=ofArray([{prefix: Unit, name: Inch, power: 2}]),
      ~toUnits=ofArray([{prefix: Unit, name: Meter, power: 2}]),
    )->Real.toFloat,
  )->toBeCloseTo(6.452e-4)
})

test("converts exponentiated units with prefixes", () => {
  expect(
    convert(
      Real.one,
      ~fromUnits=ofArray([{prefix: Milli, name: Meter, power: 2}]),
      ~toUnits=ofArray([{prefix: Unit, name: Meter, power: 2}]),
    )->Real.toFloat,
  )->toBeCloseTo(1e-6)
  expect(
    convert(
      Real.one,
      ~fromUnits=ofArray([{prefix: Kilo, name: Meter, power: 2}]),
      ~toUnits=ofArray([{prefix: Unit, name: Meter, power: 2}]),
    )->Real.toFloat,
  )->toBeCloseTo(1e6)
})

test("compound units", () => {
  expect(
    convert(
      Real.one,
      ~fromUnits=ofArray([
        {prefix: Unit, name: Meter, power: 1},
        {prefix: Unit, name: Second, power: -2},
      ]),
      ~toUnits=ofArray([
        {prefix: Unit, name: Inch, power: 1},
        {prefix: Unit, name: Hour, power: -2},
      ]),
    )->Real.toFloat,
  )->toBeCloseTo(510236220.47244096)
})

test("converts between different dimensions", () => {
  expect(
    convert(
      Real.one,
      ~fromUnits=ofArray([{prefix: Unit, name: Acre, power: 1}]),
      ~toUnits=ofArray([{prefix: Unit, name: Meter, power: 2}]),
    )->Real.toFloat,
  )->toBeCloseTo(4047.)
  expect(
    convert(
      Real.one,
      ~fromUnits=ofArray([{prefix: Unit, name: Liter, power: 1}]),
      ~toUnits=ofArray([{prefix: Unit, name: Meter, power: 3}]),
    )->Real.toFloat,
  )->toBeCloseTo(0.001)
})

test("incompatible units", () => {
  expect(
    convert(
      Real.ofInt(50),
      ~fromUnits=ofArray([{prefix: Unit, name: Celsius, power: 1}]),
      ~toUnits=ofArray([{prefix: Unit, name: Second, power: 1}]),
    ),
  )->toBe(Real.nan)
})

test("composite", () => {
  @warning("-8")
  let Some([(foot, _), (inch, _)]) = convertComposite(
    [(Real.one, {prefix: Unit, name: Meter, power: 1})],
    ~toUnits=[{prefix: Unit, name: Foot, power: 1}, {prefix: Unit, name: Inch, power: 1}],
  )

  expect(Real.toFloat(foot))->toBe(3.)
  expect(Real.toFloat(inch))->toBe(3.)
})

test("composite with negative units", () => {
  @warning("-8")
  let Some([(foot, _), (inch, _)]) = convertComposite(
    [(Real.minusOne, {prefix: Unit, name: Meter, power: 1})],
    ~toUnits=[{prefix: Unit, name: Foot, power: 1}, {prefix: Unit, name: Inch, power: 1}],
  )

  expect(Real.toFloat(foot))->toBe(-3.)
  expect(Real.toFloat(inch))->toBe(-3.)
})

test("composite with incompatible units", () => {
  let result = convertComposite(
    [(Real.one, {prefix: Unit, name: Meter, power: 1})],
    ~toUnits=[{prefix: Unit, name: Foot, power: 1}, {prefix: Unit, name: Second, power: 1}],
  )

  expect(result)->toBe(None)
})

test("composite units compatible for duplicate units", () => {
  expect(
    compositeCompatible(
      ~fromUnits=ofArray([{prefix: Unit, name: Meter, power: 1}]),
      ~toUnits=ofArray([
        {prefix: Unit, name: Foot, power: 1},
        {prefix: Unit, name: Foot, power: 1},
      ]),
    ),
  )->toBe(false)
})

test("does not crash compositeUnitsCompatible with empty toUnits", () => {
  expect(
    compositeCompatible(~fromUnits=[{prefix: Unit, name: Meter, power: 1}], ~toUnits=[]),
  )->toBe(false)
})
