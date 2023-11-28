open Jest
open PlaceholderMetadata

test("placeholder metadata", () => {
  expect(
    placeholders([
      /* 0 */ Sqrt1S,
      /* 1 */ Frac2S,
      /* 2 */ TableNS({numRows: 2, numColumns: 2}),
      /* 3 */ Arg,
      /* 4 */ Arg,
      /* 5 */ Arg,
      /* 6 */ Arg /* Table */,
      /* 7 */ Arg /* Frac numerator */,
      /* 8 */ N2_S,
      /* 9 */ Arg /* Frac */,
      /* 10 */ Arg /* Sqrt */,
    ]),
  )->toEqual([])
})
