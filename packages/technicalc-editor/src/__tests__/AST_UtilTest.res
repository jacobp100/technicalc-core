open Jest
open AST_Util

test("advance scope index", (. ()) => {
  let elements = [
    /* 0 */ AST.N1_S,
    /* 1 */ Frac2S,
    /* 2 */ N2_S,
    /* 3 */ Arg,
    /* 4 */ NLog1,
    /* 5 */ N3_S,
    /* 6 */ Arg,
    /* 7 */ N4_S,
    /* 8 */ Arg,
    /* 9 */ Superscript1,
    /* 10 */ N5_S,
    /* 11 */ Arg,
  ]

  expect(advanceScopeIndex(~direction=Forwards, elements, 0))->toEqual(Some(1))
  expect(advanceScopeIndex(~direction=Forwards, elements, 1))->toEqual(Some(9))
  expect(advanceScopeIndex(~direction=Forwards, elements, 2))->toEqual(Some(3))
  expect(advanceScopeIndex(~direction=Forwards, elements, 3))->toEqual(None)
  expect(advanceScopeIndex(~direction=Forwards, elements, 4))->toEqual(Some(7))
  expect(advanceScopeIndex(~direction=Forwards, elements, 5))->toEqual(Some(6))
  expect(advanceScopeIndex(~direction=Forwards, elements, 6))->toEqual(None)
  expect(advanceScopeIndex(~direction=Forwards, elements, 7))->toEqual(Some(8))
  expect(advanceScopeIndex(~direction=Forwards, elements, 8))->toEqual(None)
  expect(advanceScopeIndex(~direction=Forwards, elements, 9))->toEqual(Some(12))
  expect(advanceScopeIndex(~direction=Forwards, elements, 10))->toEqual(Some(11))
  expect(advanceScopeIndex(~direction=Forwards, elements, 11))->toEqual(None)
  expect(advanceScopeIndex(~direction=Forwards, elements, 12))->toEqual(None)
})

test("closest parent function function", (. ()) => {
  let elements = [
    /* 0 */ AST.N1_S,
    /* 1 */ Frac2S,
    /* 2 */ N2_S,
    /* 3 */ Arg,
    /* 4 */ NLog1,
    /* 5 */ N3_S,
    /* 6 */ Arg,
    /* 7 */ N4_S,
    /* 8 */ Arg,
    /* 9 */ Superscript1,
    /* 10 */ N5_S,
    /* 11 */ Arg,
  ]

  expect(closestParentFunction(elements, 0))->toEqual(None)
  expect(closestParentFunction(elements, 1))->toEqual(None)
  expect(closestParentFunction(elements, 2))->toEqual(Some(Frac2S, 1))
  expect(closestParentFunction(elements, 3))->toEqual(Some(Frac2S, 1))
  expect(closestParentFunction(elements, 4))->toEqual(Some(Frac2S, 1))
  expect(closestParentFunction(elements, 5))->toEqual(Some(NLog1, 4))
  expect(closestParentFunction(elements, 6))->toEqual(Some(NLog1, 4))
  expect(closestParentFunction(elements, 7))->toEqual(Some(Frac2S, 1))
  expect(closestParentFunction(elements, 8))->toEqual(Some(Frac2S, 1))
  expect(closestParentFunction(elements, 9))->toEqual(None)
  expect(closestParentFunction(elements, 10))->toEqual(Some(Superscript1, 9))
  expect(closestParentFunction(elements, 11))->toEqual(Some(Superscript1, 9))
  expect(closestParentFunction(elements, 12))->toEqual(None)
  expect(closestParentFunction(elements, 13))->toEqual(None)
})

test("recursive enclosing function", (. ()) => {
  let elements = [
    /* 0 */ AST.N1_S,
    /* 1 */ Frac2S,
    /* 2 */ N2_S,
    /* 3 */ Arg,
    /* 4 */ NLog1,
    /* 5 */ N3_S,
    /* 6 */ Arg,
    /* 7 */ N4_S,
    /* 8 */ Arg,
    /* 9 */ Superscript1,
    /* 10 */ N5_S,
    /* 11 */ Arg,
  ]

  let (_, startIndex) = closestParentFunction(elements, 5)->Belt.Option.getExn
  let (_, startIndex) = closestParentFunction(elements, startIndex)->Belt.Option.getExn

  expect(startIndex)->toBe(1)

  expect(closestParentFunction(elements, startIndex))->toBe(None)
})

test("function ranges", (. ()) => {
  let elements = [
    /* 0 */ AST.N1_S,
    /* 1 */ Frac2S,
    /* 2 */ N2_S,
    /* 3 */ Arg,
    /* 4 */ NLog1,
    /* 5 */ N3_S,
    /* 6 */ Arg,
    /* 7 */ N4_S,
    /* 8 */ Arg,
    /* 9 */ Superscript1,
    /* 10 */ N5_S,
    /* 11 */ Arg,
  ]

  expect(functionArgRangesExn(elements, 0))->toEqual([])
  expect(functionArgRangesExn(elements, 1))->toEqual([(2, 4), (4, 9)])
  expect(functionArgRangesExn(elements, 4))->toEqual([(5, 7)])
  expect(functionArgRangesExn(elements, 9))->toEqual([(10, 12)])
})
