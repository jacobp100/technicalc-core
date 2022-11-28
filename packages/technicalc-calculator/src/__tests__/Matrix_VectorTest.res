open Jest
open Scalar

test("mulVector", () => {
  expect(
    Value.mul(
      Matrix.make(~numRows=1, ~numColumns=3, [ofInt(1), ofInt(2), ofInt(3)])->Value.ofMatrix,
      Vector.make([ofInt(1), ofInt(2), ofInt(3)])->Value.ofVector,
    ),
  )->toEqual(Value.ofScalar(ofInt(14)))
})
