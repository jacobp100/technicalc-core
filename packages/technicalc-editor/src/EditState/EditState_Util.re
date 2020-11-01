let indexIsValid = (elements: array(AST.t), index) =>
  switch (Belt.Array.get(elements, index - 1)) {
  | Some(LabelS(_)) =>
    switch (Belt.Array.get(elements, index)) {
    | Some(Arg | Superscript1)
    | None => false
    | _ => true
    }
  | _ => true
  };

let preferredInsertionIndex = (~index, ~elements, ~allowLabelEditing) => {
  /* Note returning an index of length _is_ valid */
  let length = Belt.Array.length(elements);

  /* Structuring it this way fixes importing CamlPrimitive */
  let clampedIndex = max(index, 0);
  let clampedIndex = min(clampedIndex, length);

  if (allowLabelEditing) {
    clampedIndex;
  } else {
    let rec iter = index => {
      let nextIndex = index - 1;
      let canMoveIndex = nextIndex >= 0 && nextIndex <= length;
      if (canMoveIndex && !indexIsValid(elements, index)) {
        iter(nextIndex);
      } else {
        index;
      };
    };

    iter(clampedIndex);
  };
};
