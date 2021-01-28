open Jest;
open EditState;

test("should handle a single capture group", (.) => {
  let state =
    make(
      ~index=0,
      ~elements=[|
        CaptureGroupStart({placeholderMml: ""}),
        CaptureGroupEndS,
      |],
      ~allowLabelEditing=false,
    );

  expect(state.index)->toBe(1);

  let state = next(state);
  expect(state.index)->toBe(1);

  let state = previous(state);
  expect(state.index)->toBe(1);
});

test("should handle capture groups in functions", (.) => {
  let state =
    make(
      ~index=0,
      ~elements=[|
        Frac2S,
        CaptureGroupStart({placeholderMml: ""}),
        CaptureGroupEndS,
        Arg,
        CaptureGroupStart({placeholderMml: ""}),
        CaptureGroupEndS,
        Arg,
      |],
      ~allowLabelEditing=false,
    );

  expect(state.index)->toBe(0);

  let state = next(state);
  expect(state.index)->toBe(2);

  let state = next(state);
  expect(state.index)->toBe(5);

  let state = next(state);
  expect(state.index)->toBe(7);

  let state = next(state);
  expect(state.index)->toBe(7);

  let state = previous(state);
  expect(state.index)->toBe(5);

  let state = previous(state);
  expect(state.index)->toBe(2);

  let state = previous(state);
  expect(state.index)->toBe(0);

  let state = previous(state);
  expect(state.index)->toBe(0);

  expect(setIndex(state, 0).index)->toBe(0);
  expect(setIndex(state, 1).index)->toBe(2);
  expect(setIndex(state, 2).index)->toBe(2);
  expect(setIndex(state, 3).index)->toBe(2);
  expect(setIndex(state, 4).index)->toBe(5);
  expect(setIndex(state, 5).index)->toBe(5);
  expect(setIndex(state, 6).index)->toBe(5);
  expect(setIndex(state, 7).index)->toBe(7);
});

test("should handle empty capture groups with superscripts", (.) => {
  let state =
    make(
      ~index=0,
      ~elements=[|
        CaptureGroupStart({placeholderMml: ""}),
        CaptureGroupEndS,
        Superscript1,
        N2_S,
        Arg,
      |],
      ~allowLabelEditing=false,
    );

  expect(state.index)->toBe(1);

  let state = next(state);
  expect(state.index)->toBe(3);

  let state = next(state);
  expect(state.index)->toBe(4);

  let state = next(state);
  expect(state.index)->toBe(5);

  let state = next(state);
  expect(state.index)->toBe(5);

  let state = previous(state);
  expect(state.index)->toBe(4);

  let state = previous(state);
  expect(state.index)->toBe(3);

  let state = previous(state);
  expect(state.index)->toBe(1);

  let state = previous(state);
  expect(state.index)->toBe(1);

  expect(setIndex(state, 0).index)->toBe(1);
  expect(setIndex(state, 1).index)->toBe(1);
  expect(setIndex(state, 2).index)->toBe(1);
  expect(setIndex(state, 3).index)->toBe(3);
  expect(setIndex(state, 4).index)->toBe(4);
  expect(setIndex(state, 5).index)->toBe(5);
});

test("should handle filled capture groups with superscripts", (.) => {
  let state =
    make(
      ~index=0,
      ~elements=[|
        CaptureGroupStart({placeholderMml: ""}),
        N1_S,
        CaptureGroupEndS,
        Superscript1,
        N2_S,
        Arg,
      |],
      ~allowLabelEditing=false,
    );

  expect(state.index)->toBe(1);

  let state = next(state);
  expect(state.index)->toBe(2);

  let state = next(state);
  expect(state.index)->toBe(4);

  let state = next(state);
  expect(state.index)->toBe(5);

  let state = next(state);
  expect(state.index)->toBe(6);

  let state = previous(state);
  expect(state.index)->toBe(5);

  let state = previous(state);
  expect(state.index)->toBe(4);

  let state = previous(state);
  expect(state.index)->toBe(2);

  let state = previous(state);
  expect(state.index)->toBe(1);

  expect(setIndex(state, 0).index)->toBe(1);
  expect(setIndex(state, 1).index)->toBe(1);
  expect(setIndex(state, 2).index)->toBe(2);
  expect(setIndex(state, 3).index)->toBe(2);
  expect(setIndex(state, 4).index)->toBe(4);
  expect(setIndex(state, 5).index)->toBe(5);
  expect(setIndex(state, 6).index)->toBe(6);
});

test("should handle multiple capture groups in a row", (.) => {
  let state =
    make(
      ~index=0,
      ~elements=[|
        CaptureGroupStart({placeholderMml: ""}),
        CaptureGroupEndS,
        CaptureGroupStart({placeholderMml: ""}),
        CaptureGroupEndS,
        CaptureGroupStart({placeholderMml: ""}),
        CaptureGroupEndS,
      |],
      ~allowLabelEditing=false,
    );

  expect(state.index)->toBe(1);

  let state = next(state);
  expect(state.index)->toBe(3);

  let state = next(state);
  expect(state.index)->toBe(5);

  let state = next(state);
  expect(state.index)->toBe(5);

  let state = previous(state);
  expect(state.index)->toBe(3);

  let state = previous(state);
  expect(state.index)->toBe(1);

  let state = previous(state);
  expect(state.index)->toBe(1);

  expect(setIndex(state, 0).index)->toBe(1);
  expect(setIndex(state, 1).index)->toBe(1);
  expect(setIndex(state, 2).index)->toBe(3);
  expect(setIndex(state, 3).index)->toBe(3);
  expect(setIndex(state, 4).index)->toBe(5);
  expect(setIndex(state, 5).index)->toBe(5);
  expect(setIndex(state, 6).index)->toBe(5);
});

test(
  "should handle multiple capture groups in a row between non-capture group elements",
  (.) => {
    let state =
      make(
        ~index=0,
        ~elements=[|
          N1_S,
          CaptureGroupStart({placeholderMml: ""}),
          CaptureGroupEndS,
          CaptureGroupStart({placeholderMml: ""}),
          CaptureGroupEndS,
          CaptureGroupStart({placeholderMml: ""}),
          CaptureGroupEndS,
          N2_S,
        |],
        ~allowLabelEditing=false,
      );

    expect(state.index)->toBe(0);

    let state = next(state);
    expect(state.index)->toBe(2);

    let state = next(state);
    expect(state.index)->toBe(4);

    let state = next(state);
    expect(state.index)->toBe(6);

    let state = next(state);
    expect(state.index)->toBe(8);

    let state = next(state);
    expect(state.index)->toBe(8);

    let state = previous(state);
    expect(state.index)->toBe(6);

    let state = previous(state);
    expect(state.index)->toBe(4);

    let state = previous(state);
    expect(state.index)->toBe(2);

    let state = previous(state);
    expect(state.index)->toBe(0);

    let state = previous(state);
    expect(state.index)->toBe(0);
  },
);

test("should not select within a capture group when label editing", (.) => {
  let state =
    make(
      ~index=0,
      ~elements=[|
        CaptureGroupStart({placeholderMml: ""}),
        CaptureGroupEndS,
      |],
      ~allowLabelEditing=true,
    );

  expect(state.index)->toBe(0);

  let state = next(state);
  expect(state.index)->toBe(2);

  let state = previous(state);
  expect(state.index)->toBe(0);

  expect(setIndex(state, 0).index)->toBe(0);
  expect(setIndex(state, 1).index)->toBe(0);
  expect(setIndex(state, 2).index)->toBe(2);
});
