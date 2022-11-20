open Jest
open Units
open Units_ConvertChecks

test("converts simple units", () => {
  expect(
    convert(
      Value.one,
      ~fromUnits=[{prefix: Unit, type_: Meter, power: 1}],
      ~toUnits=[{prefix: Unit, type_: Inch, power: 1}],
    )->Value.toFloat,
  )->toBeCloseTo(39.37)
  expect(
    convert(
      Value.one,
      ~fromUnits=[{prefix: Unit, type_: Inch, power: 1}],
      ~toUnits=[{prefix: Unit, type_: Meter, power: 1}],
    )->Value.toFloat,
  )->toBeCloseTo(0.0254)
  expect(
    convert(
      Value.ofInt(2),
      ~fromUnits=[{prefix: Unit, type_: Meter, power: 1}],
      ~toUnits=[{prefix: Unit, type_: Inch, power: 1}],
    )->Value.toFloat,
  )->toBeCloseTo(78.74)
})

test("converts temperatures", () => {
  expect(
    convert(
      Value.ofInt(50),
      ~fromUnits=[{prefix: Unit, type_: Celsius, power: 1}],
      ~toUnits=[{prefix: Unit, type_: Fahrenheit, power: 1}],
    )->Value.toFloat,
  )->toBeCloseTo(122.)

  expect(
    convert(
      Value.ofInt(50),
      ~fromUnits=[{prefix: Unit, type_: Fahrenheit, power: 1}],
      ~toUnits=[{prefix: Unit, type_: Celsius, power: 1}],
    )->Value.toFloat,
  )->toBeCloseTo(10.)
})

test("converts exponentiated units", () => {
  expect(
    convert(
      Value.one,
      ~fromUnits=[{prefix: Unit, type_: Meter, power: 2}],
      ~toUnits=[{prefix: Unit, type_: Inch, power: 2}],
    )->Value.toFloat,
  )->toBeCloseTo(1550.)
  expect(
    convert(
      Value.one,
      ~fromUnits=[{prefix: Unit, type_: Inch, power: 2}],
      ~toUnits=[{prefix: Unit, type_: Meter, power: 2}],
    )->Value.toFloat,
  )->toBeCloseTo(6.452e-4)
})

test("converts exponentiated units with prefixes", () => {
  expect(
    convert(
      Value.one,
      ~fromUnits=[{prefix: Milli, type_: Meter, power: 2}],
      ~toUnits=[{prefix: Unit, type_: Meter, power: 2}],
    )->Value.toFloat,
  )->toBeCloseTo(1e-6)
  expect(
    convert(
      Value.one,
      ~fromUnits=[{prefix: Kilo, type_: Meter, power: 2}],
      ~toUnits=[{prefix: Unit, type_: Meter, power: 2}],
    )->Value.toFloat,
  )->toBeCloseTo(1e6)
})

test("compound units", () => {
  expect(
    convert(
      Value.one,
      ~fromUnits=[{prefix: Unit, type_: Meter, power: 1}, {prefix: Unit, type_: Second, power: -2}],
      ~toUnits=[{prefix: Unit, type_: Inch, power: 1}, {prefix: Unit, type_: Hour, power: -2}],
    )->Value.toFloat,
  )->toBeCloseTo(510236220.47244096)
})

test("converts between different dimensions", () => {
  expect(
    convert(
      Value.one,
      ~fromUnits=[{prefix: Unit, type_: Acre, power: 1}],
      ~toUnits=[{prefix: Unit, type_: Meter, power: 2}],
    )->Value.toFloat,
  )->toBeCloseTo(4047.)
  expect(
    convert(
      Value.one,
      ~fromUnits=[{prefix: Unit, type_: Liter, power: 1}],
      ~toUnits=[{prefix: Unit, type_: Meter, power: 3}],
    )->Value.toFloat,
  )->toBeCloseTo(0.001)
})

test("incompatible units", () => {
  expect(
    convert(
      Value.ofInt(50),
      ~fromUnits=[{prefix: Unit, type_: Celsius, power: 1}],
      ~toUnits=[{prefix: Unit, type_: Second, power: 1}],
    ),
  )->toBe(Value.nan)
})

test("composite", () => {
  @warning("-8")
  let Some([(foot, _), (inch, _)]) = convertComposite(
    [(Value.one, {prefix: Unit, type_: Meter, power: 1})],
    ~toUnits=[{prefix: Unit, type_: Foot, power: 1}, {prefix: Unit, type_: Inch, power: 1}],
  )

  expect(Value.toFloat(foot))->toBe(3.)
  expect(Value.toFloat(inch))->toBe(3.)
})

test("composite with negative units", () => {
  @warning("-8")
  let Some([(foot, _), (inch, _)]) = convertComposite(
    [(Value.minusOne, {prefix: Unit, type_: Meter, power: 1})],
    ~toUnits=[{prefix: Unit, type_: Foot, power: 1}, {prefix: Unit, type_: Inch, power: 1}],
  )

  expect(Value.toFloat(foot))->toBe(-3.)
  expect(Value.toFloat(inch))->toBe(-3.)
})

test("composite with incompatible units", () => {
  let result = convertComposite(
    [(Value.one, {prefix: Unit, type_: Meter, power: 1})],
    ~toUnits=[{prefix: Unit, type_: Foot, power: 1}, {prefix: Unit, type_: Second, power: 1}],
  )

  expect(result)->toBe(None)
})

test("composite units compatible for duplicate units", () => {
  expect(
    compositeUnitsCompatible(
      ~fromUnits=[{prefix: Unit, type_: Meter, power: 1}],
      ~toUnits=[{prefix: Unit, type_: Foot, power: 1}, {prefix: Unit, type_: Foot, power: 1}],
    ),
  )->toBe(false)
})

test("does not crash compositeUnitsCompatible with empty toUnits", () => {
  expect(
    compositeUnitsCompatible(~fromUnits=[{prefix: Unit, type_: Meter, power: 1}], ~toUnits=[]),
  )->toBe(false)
})
