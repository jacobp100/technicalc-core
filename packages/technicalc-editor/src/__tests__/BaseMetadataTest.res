open Jest
open BaseMetadata

test("returns none when there's no ranges", () => {
  expect(baseRanges([ConstPiS]))->toEqual(None)
  expect(baseRanges([N1_S, Add, N2_S]))->toEqual(None)
})

test("returns a single base range", () => {
  expect(baseRanges([Bin, N1_S, N2_S, Add, N3_S]))->toEqual(Some([{start: 1, end: 3, base: 2}]))
})

test("returns a base range for a single base specifier", () => {
  expect(baseRanges([Bin]))->toEqual(Some([{start: 1, end: 1, base: 2}]))
})

test("returns a multiple base ranges", () => {
  expect(baseRanges([Bin, N1_S, N2_S, Add, Hex, N3_S, N4_S, N5_S]))->toEqual(
    Some([{start: 1, end: 3, base: 2}, {start: 5, end: 8, base: 16}]),
  )
})

test("makes a best attempt at nested base ranges", () => {
  expect(baseRanges([Bin, Hex, N0_S]))->toEqual(
    Some([{start: 1, end: 1, base: 2}, {start: 2, end: 3, base: 16}]),
  )
  expect(baseRanges([Bin, N0_S, Hex, N0_S]))->toEqual(
    Some([{start: 1, end: 2, base: 2}, {start: 3, end: 4, base: 16}]),
  )
})
