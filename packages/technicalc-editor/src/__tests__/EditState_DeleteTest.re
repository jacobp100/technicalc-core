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

test("should delete labels at index", (.) => {
  let {index, elements} =
    make(
      ~index=1,
      ~elements=[|N1_S, LabelS({mml: "x"}), N2_S|],
      ~allowLabelEditing=false,
    )
    ->delete;

  expect(elements)->toEqual([|N1_S, N2_S|]);
  expect(index)->toEqual(1);
});

test("should not delete labels at index when allowing label editing", (.) => {
  let {index, elements} =
    make(
      ~index=1,
      ~elements=[|N1_S, LabelS({mml: "x"}), N2_S|],
      ~allowLabelEditing=true,
    )
    ->delete;

  expect(elements)->toEqual([|LabelS({mml: "x"}), N2_S|]);
  expect(index)->toEqual(0);
});

test("should delete labels immediately after label index", (.) => {
  let {index, elements} =
    make(
      ~index=2,
      ~elements=[|N1_S, LabelS({mml: "x"}), N2_S|],
      ~allowLabelEditing=false,
    )
    ->delete;

  expect(elements)->toEqual([|N1_S, N2_S|]);
  expect(index)->toEqual(1);
});

describe("should only delete a single label", (.) => {
  let testElements = [|
    AST.N1_S,
    LabelS({mml: "x"}),
    LabelS({mml: "y"}),
    LabelS({mml: "z"}),
    N2_S,
  |];

  test("index 1", (.) => {
    let {index, elements} =
      make(~index=1, ~elements=testElements, ~allowLabelEditing=false)
      ->delete;

    expect(elements)
    ->toEqual([|N1_S, LabelS({mml: "y"}), LabelS({mml: "z"}), N2_S|]);
    expect(index)->toEqual(1);
  });

  test("index 2", (.) => {
    let {index, elements} =
      make(~index=2, ~elements=testElements, ~allowLabelEditing=false)
      ->delete;

    expect(elements)
    ->toEqual([|N1_S, LabelS({mml: "y"}), LabelS({mml: "z"}), N2_S|]);
    expect(index)->toEqual(1);
  });

  test("index 3", (.) => {
    let {index, elements} =
      make(~index=3, ~elements=testElements, ~allowLabelEditing=false)
      ->delete;

    expect(elements)
    ->toEqual([|N1_S, LabelS({mml: "x"}), LabelS({mml: "z"}), N2_S|]);
    expect(index)->toEqual(2);
  });

  test("index 4", (.) => {
    let {index, elements} =
      make(~index=4, ~elements=testElements, ~allowLabelEditing=false)
      ->delete;

    expect(elements)
    ->toEqual([|N1_S, LabelS({mml: "x"}), LabelS({mml: "y"}), N2_S|]);
    expect(index)->toEqual(3);
  });
});
