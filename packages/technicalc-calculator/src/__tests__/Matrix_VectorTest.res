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

test("preMulVector", () => {
  expect(
    Value.mul(
      Vector.make([ofInt(1), ofInt(2), ofInt(3)])->Value.ofVector,
      Matrix.make(~numRows=1, ~numColumns=3, [ofInt(4), ofInt(5), ofInt(6)])->Value.ofMatrix,
    ),
  )->toEqual(
    Matrix.make(
      ~numRows=3,
      ~numColumns=3,
      [
        ofInt(4),
        ofInt(5),
        ofInt(6),
        ofInt(8),
        ofInt(10),
        ofInt(12),
        ofInt(12),
        ofInt(15),
        ofInt(18),
      ],
    )->Value.ofMatrix,
  )
})
