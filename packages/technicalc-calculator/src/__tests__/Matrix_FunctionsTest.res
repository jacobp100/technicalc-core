open Jest
open Matrix

let ofInt = Scalar.ofInt

test("rref", () => {
  // Square
  let matrix = make(
    ~numRows=4,
    ~numColumns=4,
    [
      ofInt(16),
      ofInt(2),
      ofInt(3),
      ofInt(13),
      ofInt(5),
      ofInt(11),
      ofInt(10),
      ofInt(8),
      ofInt(9),
      ofInt(7),
      ofInt(6),
      ofInt(12),
      ofInt(4),
      ofInt(14),
      ofInt(15),
      ofInt(1),
    ],
  )

  expect(rref(matrix))->toEqual(
    make(
      ~numRows=4,
      ~numColumns=4,
      [
        ofInt(1),
        ofInt(0),
        ofInt(0),
        ofInt(1),
        ofInt(0),
        ofInt(1),
        ofInt(0),
        ofInt(3),
        ofInt(0),
        ofInt(0),
        ofInt(1),
        ofInt(-3),
        ofInt(0),
        ofInt(0),
        ofInt(0),
        ofInt(0),
      ],
    ),
  )

  expect(rref(rref(matrix)))->toEqual(rref(matrix))

  // Non-square
  let matrix = make(
    ~numRows=2,
    ~numColumns=3,
    [ofInt(1), ofInt(3), ofInt(-1), ofInt(0), ofInt(1), ofInt(7)],
  )

  expect(rref(matrix))->toEqual(
    make(~numRows=2, ~numColumns=3, [ofInt(1), ofInt(0), ofInt(-22), ofInt(0), ofInt(1), ofInt(7)]),
  )

  expect(rref(rref(matrix)))->toEqual(rref(matrix))
})
