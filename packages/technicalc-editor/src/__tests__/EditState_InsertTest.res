open Jest
open EditState

test("should insert fraction consuming characters", (. ()) => {
  let {index, elements} =
    make(~index=1, ~elements=[N1_S, N2_S], ~formatCaptureGroups=false)->insert(Frac2S)

  expect(elements)->toEqual([Frac2S, N1_S, Arg, N2_S, Arg])
  expect(index)->toEqual(3)
})

test("should insert fraction consuming non-operator characters", (. ()) => {
  let {index, elements} =
    make(
      ~index=3,
      ~elements=[N1_S, Add, N2_S, N3_S, Add, N4_S],
      ~formatCaptureGroups=false,
    )->insert(Frac2S)

  expect(elements)->toEqual([N1_S, Add, Frac2S, N2_S, Arg, N3_S, Arg, Add, N4_S])
  expect(index)->toEqual(5)
})

test("should insert fraction inside bracket group", (. ()) => {
  let {index, elements} =
    make(
      ~index=3,
      ~elements=[N1_S, OpenBracket, N2_S, N3_S, CloseBracketS, N4_S],
      ~formatCaptureGroups=false,
    )->insert(Frac2S)

  expect(elements)->toEqual([N1_S, OpenBracket, Frac2S, N2_S, Arg, N3_S, Arg, CloseBracketS, N4_S])
  expect(index)->toEqual(5)
})

test("should move bracket groups when inserting fraction after", (. ()) => {
  let {index, elements} =
    make(
      ~index=3,
      ~elements=[OpenBracket, N1_S, CloseBracketS],
      ~formatCaptureGroups=false,
    )->insert(Frac2S)

  expect(elements)->toEqual([Frac2S, OpenBracket, N1_S, CloseBracketS, Arg, Arg])
  expect(index)->toEqual(5)
})

test("should move bracket groups when inserting fraction before", (. ()) => {
  let {index, elements} =
    make(
      ~index=0,
      ~elements=[OpenBracket, N1_S, CloseBracketS],
      ~formatCaptureGroups=false,
    )->insert(Frac2S)

  expect(elements)->toEqual([Frac2S, Arg, OpenBracket, N1_S, CloseBracketS, Arg])
  expect(index)->toEqual(1)
})

test("should move bracket groups with operators when inserting fraction before", (. ()) => {
  let {index, elements} =
    make(
      ~index=5,
      ~elements=[OpenBracket, N1_S, Add, N2_S, CloseBracketS],
      ~formatCaptureGroups=false,
    )->insert(Frac2S)

  expect(elements)->toEqual([Frac2S, OpenBracket, N1_S, Add, N2_S, CloseBracketS, Arg, Arg])
  expect(index)->toEqual(7)
})

test("should move function when inserting fraction before", (. ()) => {
  let {index, elements} =
    make(~index=2, ~elements=[Superscript1, Arg], ~formatCaptureGroups=false)->insert(Frac2S)

  expect(elements)->toEqual([Frac2S, Superscript1, Arg, Arg, Arg])
  expect(index)->toEqual(4)
})

test("should not move stationary function with no arguments when inserting fraction", (. ()) => {
  let {index, elements} =
    make(~index=1, ~elements=[SinS], ~formatCaptureGroups=false)->insert(Frac2S)

  expect(elements)->toEqual([SinS, Frac2S, Arg, Arg])
  expect(index)->toEqual(2)
})

test("should not move stationary function with arguments when inserting fraction", (. ()) => {
  let {index, elements} =
    make(~index=2, ~elements=[NLog1, Arg], ~formatCaptureGroups=false)->insert(Frac2S)

  expect(elements)->toEqual([NLog1, Arg, Frac2S, Arg, Arg])
  expect(index)->toEqual(3)
})

test("should move function when inserting fraction after", (. ()) => {
  let {index, elements} =
    make(~index=0, ~elements=[Superscript1, Arg], ~formatCaptureGroups=false)->insert(Frac2S)

  expect(elements)->toEqual([Frac2S, Arg, Superscript1, Arg, Arg])
  expect(index)->toEqual(1)
})

test("should insert fraction in 1st argument of 2ary function", (. ()) => {
  let {index, elements} =
    make(~index=1, ~elements=[RandInt2S, Arg, Arg], ~formatCaptureGroups=false)->insert(Frac2S)

  expect(elements)->toEqual([RandInt2S, Frac2S, Arg, Arg, Arg, Arg])
  expect(index)->toEqual(2)
})

test("should insert fraction in 2nd argument of 2ary function", (. ()) => {
  let {index, elements} =
    make(~index=2, ~elements=[RandInt2S, Arg, Arg], ~formatCaptureGroups=false)->insert(Frac2S)

  expect(elements)->toEqual([RandInt2S, Arg, Frac2S, Arg, Arg, Arg])
  expect(index)->toEqual(3)
})

test("should insert fraction in another fraction's numerator", (. ()) => {
  let {index, elements} =
    make(~index=1, ~elements=[Frac2S, Arg, Arg], ~formatCaptureGroups=false)->insert(Frac2S)

  expect(elements)->toEqual([Frac2S, Frac2S, Arg, Arg, Arg, Arg])
  expect(index)->toEqual(2)
})

test("should insert fraction in another fraction's denominator", (. ()) => {
  let {index, elements} =
    make(~index=2, ~elements=[Frac2S, Arg, Arg], ~formatCaptureGroups=false)->insert(Frac2S)

  expect(elements)->toEqual([Frac2S, Arg, Frac2S, Arg, Arg, Arg])
  expect(index)->toEqual(3)
})

test("should not move matrices or vectors when inserting fraction after", (. ()) => {
  let {elements} =
    make(~index=3, ~elements=[Vector2S, Arg, Arg], ~formatCaptureGroups=false)->insert(Frac2S)

  expect(elements)->toEqual([Vector2S, Arg, Arg, Frac2S, Arg, Arg])
})

test("should not move matrices or vectors when inserting fraction before", (. ()) => {
  let {elements} =
    make(~index=0, ~elements=[Vector2S, Arg, Arg], ~formatCaptureGroups=false)->insert(Frac2S)

  expect(elements)->toEqual([Frac2S, Arg, Arg, Vector2S, Arg, Arg])
})

test("should not insert iteratables inside iterator ranges", (. ()) => {
  let state = make(~index=1, ~elements=[Sum2, Arg, Arg], ~formatCaptureGroups=false)
  let result = state->insert(Sum2)

  expect(result)->toBe(state)
})

test("should insert iteratables outside of iterator ranges", (. ()) => {
  let {elements} =
    make(~index=3, ~elements=[Sum2, Arg, Arg], ~formatCaptureGroups=false)->insert(Sum2)

  expect(elements)->toEqual([Sum2, Arg, Arg, Sum2, Arg, Arg])
})

test("should not insert tables inside tables", (. ()) => {
  let state = make(~index=1, ~elements=[Vector2S, Arg, Arg], ~formatCaptureGroups=false)
  let result = state->insert(Vector2S)

  expect(result)->toBe(state)
})

test("should insert tables outside of tables", (. ()) => {
  let {elements} =
    make(~index=3, ~elements=[Vector2S, Arg, Arg], ~formatCaptureGroups=false)->insert(Vector2S)

  expect(elements)->toEqual([Vector2S, Arg, Arg, Vector2S, Arg, Arg])
})

test("should insert within capture group", (. ()) => {
  let {index, elements} =
    make(
      ~index=2,
      ~elements=[N1_S, CaptureGroupStart({placeholderMml: Some("x")}), CaptureGroupEndS, N3_S],
      ~formatCaptureGroups=false,
    )->insert(N2_S)

  expect(elements)->toEqual([
    N1_S,
    CaptureGroupStart({placeholderMml: Some("x")}),
    N2_S,
    CaptureGroupEndS,
    N3_S,
  ])
  expect(index)->toEqual(3)
})

test("should only insert within current capture group", (. ()) => {
  let testElements = [
    AST.N1_S,
    CaptureGroupStart({placeholderMml: Some("x")}),
    CaptureGroupEndS,
    CaptureGroupStart({placeholderMml: Some("y")}),
    CaptureGroupEndS,
    CaptureGroupStart({placeholderMml: Some("z")}),
    CaptureGroupEndS,
    N3_S,
  ]
  let {index, elements} =
    make(~index=4, ~elements=testElements, ~formatCaptureGroups=false)->insert(N2_S)

  expect(elements)->toEqual([
    N1_S,
    CaptureGroupStart({placeholderMml: Some("x")}),
    CaptureGroupEndS,
    CaptureGroupStart({placeholderMml: Some("y")}),
    N2_S,
    CaptureGroupEndS,
    CaptureGroupStart({placeholderMml: Some("z")}),
    CaptureGroupEndS,
    N3_S,
  ])
  expect(index)->toEqual(5)
})

test("should select first capture group when inserting array", (. ()) => {
  let {index} =
    make(~index=0, ~elements=[], ~formatCaptureGroups=false)->insertArray([
      N1_S,
      CaptureGroupStart({placeholderMml: Some("selected")}),
      CaptureGroupEndS,
      N2_S,
      CaptureGroupStart({placeholderMml: Some("not_selected")}),
      CaptureGroupEndS,
      N3_S,
    ])

  expect(index)->toBe(2)
})

test("should select first empty argument when inserting array", (. ()) => {
  let {index} =
    make(~index=0, ~elements=[], ~formatCaptureGroups=false)->insertArray([Frac2S, Arg, Arg])

  expect(index)->toBe(1)
})

test("should not select non-empty arguments when inserting array", (. ()) => {
  let {index} =
    make(~index=0, ~elements=[], ~formatCaptureGroups=false)->insertArray([Frac2S, N1_S, Arg, Arg])

  expect(index)->toBe(3)
})

test(
  "should not select non-empty arguments in functions after the first when inserting array",
  (. ()) => {
    let {index} =
      make(~index=0, ~elements=[], ~formatCaptureGroups=false)->insertArray([
        Frac2S,
        N1_S,
        Arg,
        N2_S,
        Arg,
        Superscript1,
        Arg,
      ])

    expect(index)->toBe(6)
  },
)

test(
  "should select after array when inserting array with no capture groups or placeholders",
  (. ()) => {
    let {index} =
      make(~index=0, ~elements=[], ~formatCaptureGroups=false)->insertArray([
        Frac2S,
        N1_S,
        Arg,
        N2_S,
        Arg,
        Superscript1,
        N3_S,
        Arg,
      ])

    expect(index)->toBe(8)
  },
)

test("should select after a capture group when inserting in format capture group mode", (. ()) => {
  let {index} =
    make(~index=0, ~elements=[], ~formatCaptureGroups=true)->insertArray([
      CaptureGroupStart({placeholderMml: Some("")}),
      CaptureGroupEndS,
    ])

  expect(index)->toBe(2)
})
