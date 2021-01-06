open Jest;
open EditState;

test("should handle a single label", (.) => {
  let state =
    make(
      ~index=0,
      ~elements=[|LabelS({mml: ""})|],
      ~allowLabelEditing=false,
    );

  expect(state.index)->toBe(0);

  let state = next(state);
  expect(state.index)->toBe(0);

  let state = previous(state);
  expect(state.index)->toBe(0);
});

test("should handle labels in functions", (.) => {
  let state =
    make(
      ~index=0,
      ~elements=[|Frac2S, LabelS({mml: ""}), Arg, LabelS({mml: ""}), Arg|],
      ~allowLabelEditing=false,
    );

  expect(state.index)->toBe(0);

  let state = next(state);
  expect(state.index)->toBe(1);

  let state = next(state);
  expect(state.index)->toBe(3);

  let state = next(state);
  expect(state.index)->toBe(5);

  let state = previous(state);
  expect(state.index)->toBe(3);

  let state = previous(state);
  expect(state.index)->toBe(1);

  let state = previous(state);
  expect(state.index)->toBe(0);

  expect(setIndex(state, 0).index)->toBe(0);
  expect(setIndex(state, 1).index)->toBe(1);
  expect(setIndex(state, 2).index)->toBe(1);
  expect(setIndex(state, 3).index)->toBe(3);
  expect(setIndex(state, 4).index)->toBe(3);
  expect(setIndex(state, 5).index)->toBe(5);
});

test("should handle labels with superscripts", (.) => {
  let state =
    make(
      ~index=0,
      ~elements=[|LabelS({mml: ""}), Superscript1, N2_S, Arg|],
      ~allowLabelEditing=false,
    );

  expect(state.index)->toBe(0);

  let state = next(state);
  expect(state.index)->toBe(2);

  let state = next(state);
  expect(state.index)->toBe(3);

  let state = next(state);
  expect(state.index)->toBe(4);

  let state = previous(state);
  expect(state.index)->toBe(3);

  let state = previous(state);
  expect(state.index)->toBe(2);

  let state = previous(state);
  expect(state.index)->toBe(0);

  expect(setIndex(state, 0).index)->toBe(0);
  expect(setIndex(state, 1).index)->toBe(0);
  expect(setIndex(state, 2).index)->toBe(2);
  expect(setIndex(state, 3).index)->toBe(3);
  expect(setIndex(state, 4).index)->toBe(4);
});

test("should handle multiple labels in a row", (.) => {
  let state =
    make(
      ~index=0,
      ~elements=[|
        LabelS({mml: ""}),
        LabelS({mml: ""}),
        LabelS({mml: ""}),
      |],
      ~allowLabelEditing=false,
    );

  expect(state.index)->toBe(0);

  let state = next(state);
  expect(state.index)->toBe(1);

  let state = next(state);
  expect(state.index)->toBe(2);

  let state = next(state);
  expect(state.index)->toBe(2);

  let state = previous(state);
  expect(state.index)->toBe(1);

  let state = previous(state);
  expect(state.index)->toBe(0);

  expect(setIndex(state, 0).index)->toBe(0);
  expect(setIndex(state, 1).index)->toBe(1);
  expect(setIndex(state, 2).index)->toBe(2);
  expect(setIndex(state, 3).index)->toBe(2);
});

test("should handle multiple labels in a row between non-label elements", (.) => {
  let state =
    make(
      ~index=0,
      ~elements=[|
        N1_S,
        LabelS({mml: ""}),
        LabelS({mml: ""}),
        LabelS({mml: ""}),
        N2_S,
      |],
      ~allowLabelEditing=false,
    );

  expect(state.index)->toBe(0);

  let state = next(state);
  expect(state.index)->toBe(1);

  let state = next(state);
  expect(state.index)->toBe(2);

  let state = next(state);
  expect(state.index)->toBe(3);

  let state = next(state);
  expect(state.index)->toBe(4);

  let state = next(state);
  expect(state.index)->toBe(5);

  let state = previous(state);
  expect(state.index)->toBe(4);

  let state = previous(state);
  expect(state.index)->toBe(3);

  let state = previous(state);
  expect(state.index)->toBe(2);

  let state = previous(state);
  expect(state.index)->toBe(1);

  let state = previous(state);
  expect(state.index)->toBe(0);
});
