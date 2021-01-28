open Jest;
open EditState;

test("should delete empty magnitude", (.) => {
  let {index, elements} =
    make(~index=1, ~elements=[|Magnitude1, Arg|], ~allowLabelEditing=false)
    ->delete;

  expect(elements)->toEqual([||]);
  expect(index)->toEqual(0);
});

test("should not delete filled magnitude", (.) => {
  let {index, elements} =
    make(
      ~index=1,
      ~elements=[|Magnitude1, N0_S, Arg|],
      ~allowLabelEditing=false,
    )
    ->delete;

  expect(elements)->toEqual([|Magnitude1, N0_S, Arg|]);
  expect(index)->toEqual(0);
});

test("should insert parts of fraction when deleting", (.) => {
  let {index, elements} =
    make(
      ~index=1,
      ~elements=[|Frac2S, N1_S, Arg, N2_S, Arg|],
      ~allowLabelEditing=false,
    )
    ->delete;

  expect(elements)->toEqual([|N1_S, N2_S|]);
  expect(index)->toEqual(0);
});

test("should not delete arg elements", (.) => {
  let {index, elements} =
    make(
      ~index=3,
      ~elements=[|Magnitude1, N0_S, Arg|],
      ~allowLabelEditing=false,
    )
    ->delete;

  expect(elements)->toEqual([|Magnitude1, N0_S, Arg|]);
  expect(index)->toEqual(2);
});

test("should delete empty capture groups at index", (.) => {
  let {index, elements} =
    make(
      ~index=2,
      ~elements=[|
        N1_S,
        CaptureGroupStart({placeholderMml: "x"}),
        CaptureGroupEndS,
        N2_S,
      |],
      ~allowLabelEditing=false,
    )
    ->delete;

  expect(elements)->toEqual([|N1_S, N2_S|]);
  expect(index)->toEqual(1);
});

test(
  "should not delete empty capture groups at index when allowing label editing",
  (.) => {
  let {index, elements} =
    make(
      ~index=1,
      ~elements=[|
        N1_S,
        CaptureGroupStart({placeholderMml: "x"}),
        CaptureGroupEndS,
        N2_S,
      |],
      ~allowLabelEditing=true,
    )
    ->delete;

  expect(elements)
  ->toEqual([|
      CaptureGroupStart({placeholderMml: "x"}),
      CaptureGroupEndS,
      N2_S,
    |]);
  expect(index)->toEqual(0);
});

test(
  "should delete empty capture groups before index when allowing label editing",
  (.) => {
  let {index, elements} =
    make(
      ~index=3,
      ~elements=[|
        N1_S,
        CaptureGroupStart({placeholderMml: "x"}),
        CaptureGroupEndS,
        N2_S,
      |],
      ~allowLabelEditing=true,
    )
    ->delete;

  expect(elements)->toEqual([|N1_S, N2_S|]);
  expect(index)->toEqual(1);
});

test("should delete empty capture groups immediately after label index", (.) => {
  let {index, elements} =
    make(
      ~index=2,
      ~elements=[|
        N1_S,
        CaptureGroupStart({placeholderMml: "x"}),
        CaptureGroupEndS,
        N2_S,
      |],
      ~allowLabelEditing=false,
    )
    ->delete;

  expect(elements)->toEqual([|N1_S, N2_S|]);
  expect(index)->toEqual(1);
});

describe("should only delete a single capture empty group", (.) => {
  let testElements = [|
    AST.N1_S,
    CaptureGroupStart({placeholderMml: "x"}),
    CaptureGroupEndS,
    CaptureGroupStart({placeholderMml: "y"}),
    CaptureGroupEndS,
    CaptureGroupStart({placeholderMml: "z"}),
    CaptureGroupEndS,
    N2_S,
  |];

  test("capture group 1", (.) => {
    let {index, elements} =
      make(~index=2, ~elements=testElements, ~allowLabelEditing=false)
      ->delete;

    expect(elements)
    ->toEqual([|
        N1_S,
        CaptureGroupStart({placeholderMml: "y"}),
        CaptureGroupEndS,
        CaptureGroupStart({placeholderMml: "z"}),
        CaptureGroupEndS,
        N2_S,
      |]);
    expect(index)->toEqual(2);
  });

  test("capture group 2", (.) => {
    let {index, elements} =
      make(~index=4, ~elements=testElements, ~allowLabelEditing=false)
      ->delete;

    expect(elements)
    ->toEqual([|
        N1_S,
        CaptureGroupStart({placeholderMml: "x"}),
        CaptureGroupEndS,
        CaptureGroupStart({placeholderMml: "z"}),
        CaptureGroupEndS,
        N2_S,
      |]);
    expect(index)->toEqual(4);
  });

  test("capture group 3", (.) => {
    let {index, elements} =
      make(~index=6, ~elements=testElements, ~allowLabelEditing=false)
      ->delete;

    expect(elements)
    ->toEqual([|
        N1_S,
        CaptureGroupStart({placeholderMml: "x"}),
        CaptureGroupEndS,
        CaptureGroupStart({placeholderMml: "y"}),
        CaptureGroupEndS,
        N2_S,
      |]);
    expect(index)->toEqual(4);
  });
});
