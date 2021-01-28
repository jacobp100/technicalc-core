let isEmptyCaptureGroup = (elements, index) =>
  switch (Belt.Array.get(elements, index)) {
  | Some(AST.CaptureGroupStart(_)) =>
    switch (Belt.Array.get(elements, index + 1)) {
    | Some(CaptureGroupEndS) => true
    | _ => false
    }
  | _ => false
  };

type preferredShiftDirection =
  | NoShift
  | Forwards
  | Backwards;

let preferredShiftDirection =
    (~index, ~elements: array(AST.t), ~allowLabelEditing) => {
  let previousElement = Belt.Array.get(elements, index - 1);

  if (!allowLabelEditing) {
    let currentElement = Belt.Array.get(elements, index);

    switch (previousElement, currentElement) {
    | (_, Some(CaptureGroupStart(_))) => Forwards
    | (Some(CaptureGroupEndS), _) => Backwards
    | _ => NoShift
    };
  } else {
    switch (previousElement) {
    | Some(CaptureGroupStart(_)) => Backwards
    | _ => NoShift
    };
  };
};

let preferredInsertionIndex = (~index, ~elements, ~allowLabelEditing) => {
  /* Note returning an index of length _is_ valid */
  let length = Belt.Array.length(elements);

  /* Structuring it this way fixes importing CamlPrimitive */
  let clampedIndex = max(index, 0);
  let clampedIndex = min(clampedIndex, length);

  let rec iter = index =>
    switch (preferredShiftDirection(~index, ~elements, ~allowLabelEditing)) {
    | Backwards =>
      let nextIndex = index - 1;
      let canMoveIndex = nextIndex >= 0;
      canMoveIndex ? iter(nextIndex) : index;
    | Forwards =>
      let nextIndex = index + 1;
      let canMoveIndex = nextIndex < length;
      canMoveIndex ? iter(nextIndex) : index;
    | NoShift => index
    };

  iter(clampedIndex);
};
