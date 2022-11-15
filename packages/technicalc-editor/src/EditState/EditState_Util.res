let isEmptyCaptureGroup = (elements, index) =>
  switch Belt.Array.get(elements, index) {
  | Some(AST.CaptureGroupStart(_)) =>
    switch Belt.Array.get(elements, index + 1) {
    | Some(CaptureGroupEndS) => true
    | _ => false
    }
  | _ => false
  }

type direction =
  | Forwards
  | Backwards

let preferredShiftDirection = (~index, ~elements, ~formatCaptureGroups) =>
  if !formatCaptureGroups {
    let previousIndex = index - 1

    switch Belt.Array.get(elements, index) {
    | Some(AST.CaptureGroupStart(_)) => Some(Forwards)
    | _ =>
      switch Belt.Array.get(elements, previousIndex) {
      | Some(CaptureGroupEndS) => Some(Backwards)
      | _ => None
      }
    }
  } else if isEmptyCaptureGroup(elements, index - 1) {
    Some(Backwards)
  } else {
    None
  }

let preferredInsertionIndex = (~index, ~elements, ~formatCaptureGroups) => {
  /* Note returning an index of length _is_ valid */
  let length = Belt.Array.length(elements)

  /* Structuring it this way fixes importing CamlPrimitive */
  let clampedIndex = max(index, 0)
  let clampedIndex = min(clampedIndex, length)

  let rec iter = index => {
    let nextIndex = switch preferredShiftDirection(~index, ~elements, ~formatCaptureGroups) {
    | Some(Backwards) => Some(index - 1)
    | Some(Forwards) => Some(index + 1)
    | None => None
    }
    switch nextIndex {
    | Some(nextIndex) if nextIndex >= 0 && nextIndex <= length => iter(nextIndex)
    | _ => index
    }
  }

  iter(clampedIndex)
}
