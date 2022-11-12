open Jest
open AST_Util

test("arg end index", (. ()) => {
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

  expect(argEndIndex(elements, 0))->toBe(9)
  expect(argEndIndex(elements, 1))->toBe(9)
  expect(argEndIndex(elements, 2))->toBe(4)
  expect(argEndIndex(elements, 3))->toBe(4)
  expect(argEndIndex(elements, 4))->toBe(7)
  expect(argEndIndex(elements, 5))->toBe(7)
  expect(argEndIndex(elements, 6))->toBe(7)
  expect(argEndIndex(elements, 7))->toBe(9)
  expect(argEndIndex(elements, 8))->toBe(9)
  expect(argEndIndex(elements, 9))->toBe(12)
  expect(argEndIndex(elements, 10))->toBe(12)
  expect(argEndIndex(elements, 11))->toBe(12)
  expect(argEndIndex(elements, 12))->toBe(12)
  expect(argEndIndex(elements, 13))->toBe(13)
})

test("enclosing function", (. ()) => {
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

  expect(enclosingFunction(elements, 0))->toEqual(None)
  expect(enclosingFunction(elements, 1))->toEqual(None)
  expect(enclosingFunction(elements, 2))->toEqual(Some(Frac2S, 1, 9))
  expect(enclosingFunction(elements, 3))->toEqual(Some(Frac2S, 1, 9))
  expect(enclosingFunction(elements, 4))->toEqual(Some(Frac2S, 1, 9))
  expect(enclosingFunction(elements, 5))->toEqual(Some(NLog1, 4, 7))
  expect(enclosingFunction(elements, 6))->toEqual(Some(NLog1, 4, 7))
  expect(enclosingFunction(elements, 7))->toEqual(Some(Frac2S, 1, 9))
  expect(enclosingFunction(elements, 8))->toEqual(Some(Frac2S, 1, 9))
  expect(enclosingFunction(elements, 9))->toEqual(None)
  expect(enclosingFunction(elements, 10))->toEqual(Some(Superscript1, 9, 12))
  expect(enclosingFunction(elements, 11))->toEqual(Some(Superscript1, 9, 12))
  expect(enclosingFunction(elements, 12))->toEqual(None)
  expect(enclosingFunction(elements, 13))->toEqual(None)
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

  let (_, startIndex, _) = enclosingFunction(elements, 5)->Belt.Option.getExn
  let (_, startIndex, _) = enclosingFunction(elements, startIndex)->Belt.Option.getExn

  expect(startIndex)->toBe(1)

  expect(enclosingFunction(elements, startIndex))->toBe(None)
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

  expect(functionArgRanges(elements, 1))->toEqual([(2, 3), (4, 8)])
  expect(functionArgRanges(elements, 4))->toEqual([(5, 6)])
  expect(functionArgRanges(elements, 9))->toEqual([(10, 11)])
})
