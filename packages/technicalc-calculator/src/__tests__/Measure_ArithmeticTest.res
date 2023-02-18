open Jest
open Measure

test("add", () => {
  expect(
    add(
      ofReal(Real.one, ~units=[{prefix: Unit, name: Meter, power: 1}]),
      ofReal(Real.one, ~units=[{prefix: Unit, name: Meter, power: 1}]),
    ),
  )->toEqual(ofReal(Real.ofInt(2), ~units=[{prefix: Unit, name: Meter, power: 1}]))

  expect(
    add(
      ofReal(Real.one, ~units=[{prefix: Unit, name: Foot, power: 1}]),
      ofReal(Real.one, ~units=[{prefix: Unit, name: Inch, power: 1}]),
    ),
  )->toEqual(ofReal(Real.ofFloat(0.3302), ~units=[{prefix: Unit, name: Meter, power: 1}]))
})

test("mul", () => {
  expect(
    mul(
      ofReal(Real.one, ~units=[{prefix: Unit, name: Meter, power: 1}]),
      ofReal(Real.one, ~units=[{prefix: Unit, name: Meter, power: 1}]),
    ),
  )->toEqual(ofReal(Real.ofInt(1), ~units=[{prefix: Unit, name: Meter, power: 2}]))

  expect(
    mul(
      ofReal(Real.one, ~units=[{prefix: Unit, name: Foot, power: 1}]),
      ofReal(Real.one, ~units=[{prefix: Unit, name: Inch, power: 1}]),
    ),
  )->toEqual(
    ofReal(
      Real.one,
      ~units=[{prefix: Unit, name: Foot, power: 1}, {prefix: Unit, name: Inch, power: 1}],
    ),
  )
})

test("div", () => {
  expect(
    div(
      ofReal(Real.one, ~units=[{prefix: Unit, name: Meter, power: 1}]),
      ofReal(
        Real.one,
        ~units=[{prefix: Unit, name: Meter, power: 1}, {prefix: Unit, name: Second, power: -1}],
      ),
    ),
  )->toEqual(ofReal(Real.ofInt(1), ~units=[{prefix: Unit, name: Second, power: 1}]))
})
