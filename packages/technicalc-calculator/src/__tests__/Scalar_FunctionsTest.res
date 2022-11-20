open Jest
open Scalar

test("logBase with precision loss", () => {
  expect(logBase(~base=ofInt(2), ofInt(64)))->toEqual(ofReal(Rational(6, 1, Unit)))
})
