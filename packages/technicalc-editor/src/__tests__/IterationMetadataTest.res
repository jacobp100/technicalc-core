open Jest
open IterationMetadata

test("differential", () => {
  expect(
    iterationRanges([
      /* 0 */ N1_S,
      /* 1 */ Add,
      /* 2 */ Differential2,
      /* 3 */ N2_S,
      /* 4 */ Arg,
      /* 5 */ N3_S,
      /* 6 */ Arg,
      /* 7 */ Add,
      /* 8 */ N4_S,
    ]),
  )->toEqual(list{(3, 4)})
})

test("integral", () => {
  expect(
    iterationRanges([
      /* 0 */ N1_S,
      /* 1 */ Add,
      /* 2 */ Integral3,
      /* 3 */ N2_S,
      /* 4 */ Arg,
      /* 5 */ N3_S,
      /* 6 */ Arg,
      /* 7 */ N4_S,
      /* 8 */ Arg,
      /* 9 */ Add,
      /* 10 */ N5_S,
    ]),
  )->toEqual(list{(7, 8)})
})

test("sum as implicit operator", () => {
  expect(
    iterationRanges([
      /* 0 */ N1_S,
      /* 1 */ Add,
      /* 2 */ Sum2,
      /* 3 */ N1_S,
      /* 4 */ Arg,
      /* 5 */ N1_S,
      /* 6 */ N0_S,
      /* 7 */ Arg,
      /* 8 */ XS,
      /* 9 */ Superscript1,
      /* 10 */ N2_S,
      /* 11 */ Arg,
      /* 12 */ Add,
      /* 13 */ N3_S,
      /* 14 */ Add,
      /* 15 */ N4_S,
    ]),
  )->toEqual(list{(8, 12)})
})

test("sum as explicit function", () => {
  expect(
    iterationRanges([
      /* 0 */ N1_S,
      /* 1 */ Add,
      /* 2 */ Sum2,
      /* 3 */ N1_S,
      /* 4 */ Arg,
      /* 5 */ N1_S,
      /* 6 */ N0_S,
      /* 7 */ Arg,
      /* 8 */ OpenBracket,
      /* 9 */ XS,
      /* 10 */ Superscript1,
      /* 11 */ N2_S,
      /* 12 */ Arg,
      /* 13 */ Add,
      /* 14 */ N3_S,
      /* 15 */ CloseBracketS,
      /* 16 */ Add,
      /* 17 */ N4_S,
    ]),
  )->toEqual(list{(8, 15)})
})

test("multiple sums", () => {
  expect(
    iterationRanges([
      /* 0 */ N1_S,
      /* 1 */ Add,
      /* 2 */ Sum2,
      /* 3 */ N1_S,
      /* 4 */ Arg,
      /* 5 */ N1_S,
      /* 6 */ N0_S,
      /* 7 */ Arg,
      /* 2 */ Sum2,
      /* 3 */ N1_S,
      /* 4 */ Arg,
      /* 5 */ N1_S,
      /* 6 */ N0_S,
      /* 7 */ Arg,
      /* 8 */ OpenBracket,
      /* 9 */ XS,
      /* 10 */ Superscript1,
      /* 11 */ N2_S,
      /* 12 */ Arg,
      /* 13 */ Add,
      /* 14 */ N3_S,
      /* 15 */ CloseBracketS,
      /* 16 */ Add,
      /* 17 */ N4_S,
    ]),
  )->toEqual(list{(14, 21), (8, 22)})
})

test("sum as implicit operator immediately followed by operator", () => {
  expect(
    iterationRanges([
      /* 0 */ N1_S,
      /* 1 */ Add,
      /* 2 */ Sum2,
      /* 3 */ N1_S,
      /* 4 */ Arg,
      /* 5 */ N1_S,
      /* 6 */ N0_S,
      /* 7 */ Arg,
      /* 8 */ Add,
    ]),
  )->toEqual(list{(8, 8)})
})
