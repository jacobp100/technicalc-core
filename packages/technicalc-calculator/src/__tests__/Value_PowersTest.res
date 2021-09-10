open Jest
open Value

test("sqrt via power", (. ()) => {
  let half = ofInt(1) / ofInt(2)
  let sqrt2 = ofReal(Real.ofRational(1, 1, Sqrt(2)))
  let ofString = x => ofString(x)->Belt.Option.getExn

  expect(ofInt(16) ** half)->toEqual(ofInt(4))
  expect(ofInt(-16) ** half)->toEqual(ofInt(4) * i)

  expect(ofInt(8) ** half)->toEqual(ofInt(2) * sqrt2)
  expect(ofInt(-8) ** half)->toEqual(ofInt(2) * sqrt2 * i)

  expect(ofString("1e40") ** half)->toEqual(ofString("1e20"))
})

test("real to integer power", (. ()) => {
  expect(ofInt(2) ** ofInt(2))->toEqual(ofInt(4))
})

test("imaginary to integer power", (. ()) => {
  expect(i ** ofInt(1))->toEqual(i)
  expect(i ** ofInt(2))->toEqual(minusOne)
  expect(i ** ofInt(3))->toEqual(minusI)
  expect(i ** ofInt(4))->toEqual(one)
})

test("percentages", (. ()) => {
  expect(ofPercent(Scalar.ofInt(50)) ** ofInt(2))->toEqual(ofFloat(0.25))
  expect(ofInt(2) ** ofPercent(Scalar.ofInt(50)))->toEqual(sqrt(ofInt(2)))
  expect(ofPercent(Scalar.ofInt(50)) ** ofPercent(Scalar.ofInt(50)))->toEqual(one / sqrt(ofInt(2)))
})
