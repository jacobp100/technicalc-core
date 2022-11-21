open Jest
open Scalar
open Matrix

test("mutliply square matrices", () => {
  expect(
    mul(
      make(~numRows=2, ~numColumns=2, [ofInt(1), ofInt(2), ofInt(3), ofInt(4)]),
      make(~numRows=2, ~numColumns=2, [ofInt(1), ofInt(2), ofInt(3), ofInt(4)]),
    ),
  )->toEqual(make(~numRows=2, ~numColumns=2, [ofInt(7), ofInt(10), ofInt(15), ofInt(22)]))
})

test("mutliply non-square matrices", () => {
  expect(
    mul(
      make(~numRows=2, ~numColumns=3, [ofInt(1), ofInt(2), ofInt(3), ofInt(4), ofInt(5), ofInt(6)]),
      make(~numRows=3, ~numColumns=2, [ofInt(1), ofInt(2), ofInt(3), ofInt(4), ofInt(5), ofInt(6)]),
    ),
  )->toEqual(make(~numRows=2, ~numColumns=2, [ofInt(22), ofInt(28), ofInt(49), ofInt(64)]))

  // https://www.wolframalpha.com/input?key=&i=%7B%7B1%2C2%2C3%7D%2C%7B4%2C5%2C6%7D%2C%7B7%2C8%2C9%7D%2C%7B10%2C11%2C12%7D%2C%7B13%2C14%2C15%7D%7D+*+%7B%7B1%2C2%2C3%2C4%7D%2C%7B5%2C6%2C7%2C8%7D%2C%7B9%2C10%2C11%2C12%7D%7D
  expect(
    mul(
      make(
        ~numRows=5,
        ~numColumns=3,
        [
          ofInt(1),
          ofInt(2),
          ofInt(3),
          ofInt(4),
          ofInt(5),
          ofInt(6),
          ofInt(7),
          ofInt(8),
          ofInt(9),
          ofInt(10),
          ofInt(11),
          ofInt(12),
          ofInt(13),
          ofInt(14),
          ofInt(15),
        ],
      ),
      make(~numRows=3, ~numColumns=4, [ofInt(1), ofInt(2), ofInt(3), ofInt(4), ofInt(5), ofInt(6)]),
    ),
  )->toEqual(
    make(
      ~numRows=2,
      ~numColumns=2,
      [
        ofInt(38),
        ofInt(44),
        ofInt(50),
        ofInt(56),
        ofInt(83),
        ofInt(98),
        ofInt(113),
        ofInt(128),
        ofInt(128),
        ofInt(152),
        ofInt(176),
        ofInt(200),
        ofInt(173),
        ofInt(206),
        ofInt(239),
        ofInt(272),
        ofInt(218),
        ofInt(260),
        ofInt(302),
        ofInt(344),
      ],
    ),
  )
})
