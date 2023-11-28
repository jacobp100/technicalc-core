open Jest
open AST_NormalizationContext

test("insert table", () => {
  expect(noTablePermittedRanges([Frac2S, Arg, Arg]))->toEqual(list{(2, 2)})
  expect(noTablePermittedRanges([Sqrt1S, Frac2S, Arg, Arg, Arg]))->toEqual(list{(1, 4)})
})
