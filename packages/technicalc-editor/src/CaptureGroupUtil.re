type captureGroup = {
  placeholderMml: string,
  elements: array(AST.t),
};

let isInCaptureGroup = (s: EditState.t) =>
  !s.allowLabelEditing
    ? EditState_Util.isEmptyCaptureGroup(s.elements, s.index - 1) : false;

let captureGroups = (elements: array(AST.t)): array(captureGroup) => {
  let rec iter = (~captureGroupStartStack=[], ~captureGroups=[], i) =>
    switch (Belt.Array.get(elements, i)) {
    | Some(CaptureGroupStart({placeholderMml})) =>
      let captureGroupStartStack = [
        (i + 1, placeholderMml),
        ...captureGroupStartStack,
      ];
      iter(~captureGroupStartStack, ~captureGroups, i + 1);
    | Some(CaptureGroupEndS) =>
      switch (captureGroupStartStack) {
      | [(start, placeholderMml), ...captureGroupStartStack] =>
        let elements =
          Belt.Array.slice(elements, ~offset=start, ~len=i - start)
          ->AST_Types.normalize;
        let captureGroups =
          Belt.Array.length(elements) != 0
            ? [{placeholderMml, elements}, ...captureGroups] : captureGroups;
        iter(~captureGroupStartStack, ~captureGroups, i + 1);
      | [] => iter(~captureGroupStartStack, ~captureGroups, i + 1)
      }
    | Some(_) => iter(~captureGroupStartStack, ~captureGroups, i + 1)
    | None => Belt.List.toArray(captureGroups)->ArrayUtil.reverseInPlace
    };

  iter(0);
};
