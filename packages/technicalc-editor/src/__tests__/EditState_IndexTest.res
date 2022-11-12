open Jest
open EditState

test("should handle a single capture group", (. ()) => {
  let state = make(
    ~index=0,
    ~elements=[CaptureGroupStart({placeholderMml: Some("")}), CaptureGroupEndS],
    ~formatCaptureGroups=false,
  )

  expect(state.index)->toBe(1)

  let state = next(state)
  expect(state.index)->toBe(1)

  let state = previous(state)
  expect(state.index)->toBe(1)
})

test("should handle capture groups in functions", (. ()) => {
  let state = make(
    ~index=0,
    ~elements=[
      Frac2S,
      CaptureGroupStart({placeholderMml: Some("")}),
      CaptureGroupEndS,
      Arg,
      CaptureGroupStart({placeholderMml: Some("")}),
      CaptureGroupEndS,
      Arg,
    ],
    ~formatCaptureGroups=false,
  )

  expect(state.index)->toBe(0)

  let state = next(state)
  expect(state.index)->toBe(2)

  let state = next(state)
  expect(state.index)->toBe(5)

  let state = next(state)
  expect(state.index)->toBe(7)

  let state = next(state)
  expect(state.index)->toBe(7)

  let state = previous(state)
  expect(state.index)->toBe(5)

  let state = previous(state)
  expect(state.index)->toBe(2)

  let state = previous(state)
  expect(state.index)->toBe(0)

  let state = previous(state)
  expect(state.index)->toBe(0)

  expect(setIndex(state, 0).index)->toBe(0)
  expect(setIndex(state, 1).index)->toBe(2)
  expect(setIndex(state, 2).index)->toBe(2)
  expect(setIndex(state, 3).index)->toBe(2)
  expect(setIndex(state, 4).index)->toBe(5)
  expect(setIndex(state, 5).index)->toBe(5)
  expect(setIndex(state, 6).index)->toBe(5)
  expect(setIndex(state, 7).index)->toBe(7)
})

test("should handle empty capture groups with superscripts", (. ()) => {
  let state = make(
    ~index=0,
    ~elements=[
      CaptureGroupStart({placeholderMml: Some("")}),
      CaptureGroupEndS,
      Superscript1,
      N2_S,
      Arg,
    ],
    ~formatCaptureGroups=false,
  )

  expect(state.index)->toBe(1)

  let state = next(state)
  expect(state.index)->toBe(3)

  let state = next(state)
  expect(state.index)->toBe(4)

  let state = next(state)
  expect(state.index)->toBe(5)

  let state = next(state)
  expect(state.index)->toBe(5)

  let state = previous(state)
  expect(state.index)->toBe(4)

  let state = previous(state)
  expect(state.index)->toBe(3)

  let state = previous(state)
  expect(state.index)->toBe(1)

  let state = previous(state)
  expect(state.index)->toBe(1)

  expect(setIndex(state, 0).index)->toBe(1)
  expect(setIndex(state, 1).index)->toBe(1)
  expect(setIndex(state, 2).index)->toBe(1)
  expect(setIndex(state, 3).index)->toBe(3)
  expect(setIndex(state, 4).index)->toBe(4)
  expect(setIndex(state, 5).index)->toBe(5)
})

test("should handle filled capture groups with superscripts", (. ()) => {
  let state = make(
    ~index=0,
    ~elements=[
      CaptureGroupStart({placeholderMml: Some("")}),
      N1_S,
      CaptureGroupEndS,
      Superscript1,
      N2_S,
      Arg,
    ],
    ~formatCaptureGroups=false,
  )

  expect(state.index)->toBe(1)

  let state = next(state)
  expect(state.index)->toBe(2)

  let state = next(state)
  expect(state.index)->toBe(4)

  let state = next(state)
  expect(state.index)->toBe(5)

  let state = next(state)
  expect(state.index)->toBe(6)

  let state = previous(state)
  expect(state.index)->toBe(5)

  let state = previous(state)
  expect(state.index)->toBe(4)

  let state = previous(state)
  expect(state.index)->toBe(2)

  let state = previous(state)
  expect(state.index)->toBe(1)

  expect(setIndex(state, 0).index)->toBe(1)
  expect(setIndex(state, 1).index)->toBe(1)
  expect(setIndex(state, 2).index)->toBe(2)
  expect(setIndex(state, 3).index)->toBe(2)
  expect(setIndex(state, 4).index)->toBe(4)
  expect(setIndex(state, 5).index)->toBe(5)
  expect(setIndex(state, 6).index)->toBe(6)
})

test("should handle multiple capture groups in a row", (. ()) => {
  let state = make(
    ~index=0,
    ~elements=[
      CaptureGroupStart({placeholderMml: Some("")}),
      CaptureGroupEndS,
      CaptureGroupStart({placeholderMml: Some("")}),
      CaptureGroupEndS,
      CaptureGroupStart({placeholderMml: Some("")}),
      CaptureGroupEndS,
    ],
    ~formatCaptureGroups=false,
  )

  expect(state.index)->toBe(1)

  let state = next(state)
  expect(state.index)->toBe(3)

  let state = next(state)
  expect(state.index)->toBe(5)

  let state = next(state)
  expect(state.index)->toBe(5)

  let state = previous(state)
  expect(state.index)->toBe(3)

  let state = previous(state)
  expect(state.index)->toBe(1)

  let state = previous(state)
  expect(state.index)->toBe(1)

  expect(setIndex(state, 0).index)->toBe(1)
  expect(setIndex(state, 1).index)->toBe(1)
  expect(setIndex(state, 2).index)->toBe(3)
  expect(setIndex(state, 3).index)->toBe(3)
  expect(setIndex(state, 4).index)->toBe(5)
  expect(setIndex(state, 5).index)->toBe(5)
  expect(setIndex(state, 6).index)->toBe(5)
})

test(
  "should handle multiple capture groups in a row between non-capture group elements",
  (. ()) => {
    let state = make(
      ~index=0,
      ~elements=[
        N1_S,
        CaptureGroupStart({placeholderMml: Some("")}),
        CaptureGroupEndS,
        CaptureGroupStart({placeholderMml: Some("")}),
        CaptureGroupEndS,
        CaptureGroupStart({placeholderMml: Some("")}),
        CaptureGroupEndS,
        N2_S,
      ],
      ~formatCaptureGroups=false,
    )

    expect(state.index)->toBe(0)

    let state = next(state)
    expect(state.index)->toBe(2)

    let state = next(state)
    expect(state.index)->toBe(4)

    let state = next(state)
    expect(state.index)->toBe(6)

    let state = next(state)
    expect(state.index)->toBe(8)

    let state = next(state)
    expect(state.index)->toBe(8)

    let state = previous(state)
    expect(state.index)->toBe(6)

    let state = previous(state)
    expect(state.index)->toBe(4)

    let state = previous(state)
    expect(state.index)->toBe(2)

    let state = previous(state)
    expect(state.index)->toBe(0)

    let state = previous(state)
    expect(state.index)->toBe(0)
  },
)

test("should not select within a capture group when label editing", (. ()) => {
  let state = make(
    ~index=0,
    ~elements=[CaptureGroupStart({placeholderMml: Some("")}), CaptureGroupEndS],
    ~formatCaptureGroups=true,
  )

  expect(state.index)->toBe(0)

  let state = next(state)
  expect(state.index)->toBe(2)

  let state = previous(state)
  expect(state.index)->toBe(0)

  expect(setIndex(state, 0).index)->toBe(0)
  expect(setIndex(state, 1).index)->toBe(0)
  expect(setIndex(state, 2).index)->toBe(2)
})

test("up", (. ()) => {
  let elements = [
    /* 0 */ AST.N1_S,
    /* 1 */ Frac2S,
    /* 2 */ N2_S,
    /* 3 */ Arg,
    /* 4 */ N3_S,
    /* 5 */ NLog1,
    /* 6 */ N4_S,
    /* 7 */ Arg,
    /* 8 */ N5_S,
    /* 9 */ Arg,
    /* 10 */ Superscript1,
    /* 11 */ N6_S,
    /* 12 */ Arg,
  ]

  let state = make(~index=8, ~elements, ~formatCaptureGroups=true)
  expect(state.index)->toBe(8)

  let state = moveUp(state)
  expect(state.index)->toBe(3)

  let state = moveUp(state)
  expect(state.index)->toBe(3)
})

test("down", (. ()) => {
  let elements = [
    /* 0 */ AST.N1_S,
    /* 1 */ Frac2S,
    /* 2 */ N2_S,
    /* 3 */ Arg,
    /* 4 */ N3_S,
    /* 5 */ NLog1,
    /* 6 */ N4_S,
    /* 7 */ Arg,
    /* 8 */ N5_S,
    /* 9 */ Arg,
    /* 10 */ Superscript1,
    /* 11 */ N6_S,
    /* 12 */ Arg,
  ]

  let state = make(~index=3, ~elements, ~formatCaptureGroups=true)
  expect(state.index)->toBe(3)

  let state = moveDown(state)
  expect(state.index)->toBe(4)

  let state = moveDown(state)
  expect(state.index)->toBe(4)
})

test("up/down in specific elements", (. ()) => {
  let elements = [
    /* 0 */ AST.TableNS({numRows: 2, numColumns: 2}),
    /* 1 */ N1_S,
    /* 2 */ Arg,
    /* 3 */ N2_S,
    /* 4 */ Arg,
    /* 5 */ N3_S,
    /* 6 */ Arg,
    /* 7 */ N4_S,
    /* 8 */ Arg,
  ]

  let state = make(~index=3, ~elements, ~formatCaptureGroups=true)
  expect(state.index)->toBe(3)

  let state = moveDown(state)
  expect(state.index)->toBe(7)

  let state = moveUp(state)
  expect(state.index)->toBe(4)
})
