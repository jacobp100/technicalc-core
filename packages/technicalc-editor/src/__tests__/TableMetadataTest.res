open Jest
open TableMetadata

test("Table", () => {
  expect(
    tableRanges([
      /* 0 */ N1_S,
      /* 1 */ TableNS({numRows: 2, numColumns: 2}),
      /* 2 */ N1_S,
      /* 3 */ Arg,
      /* 4 */ N1_S,
      /* 5 */ Arg,
      /* 6 */ N1_S,
      /* 7 */ Arg,
      /* 8 */ N1_S,
      /* 9 */ Arg,
      /* 10 */ N1_S,
    ]),
  )->toEqual(list{(2, 9)})
})
