type captureGroup = {
  placeholderMml: string,
  elements: array(AST.t),
};

let%private elementsEqU = (. a, b) => AST.eq(a, b);

let%private contains = (captureGroups, ~placeholderMml, ~elements) =>
  Belt.List.someU(captureGroups, (. x) => {
    placeholderMml == x.placeholderMml
    && Belt.Array.eqU(elements, x.elements, elementsEqU)
  });

let populatedCaptureGroups = (elements: array(AST.t)): array(captureGroup) => {
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
          && !contains(captureGroups, ~placeholderMml, ~elements)
            ? [{placeholderMml, elements}, ...captureGroups] : captureGroups;

        iter(~captureGroupStartStack, ~captureGroups, i + 1);
      | [] => iter(~captureGroupStartStack, ~captureGroups, i + 1)
      }
    | Some(_) => iter(~captureGroupStartStack, ~captureGroups, i + 1)
    | None => Belt.List.toArray(captureGroups)->ArrayUtil.reverseInPlace
    };

  iter(0);
};

let emptyCaptureGroups = (elements: array(AST.t)) => {
  let length = Belt.Array.length(elements);

  let rec iter = (~captureGroups=[], i) =>
    if (i >= length) {
      Belt.List.toArray(captureGroups)->ArrayUtil.reverseInPlace;
    } else {
      let captureGroups =
        EditState_Util.isEmptyCaptureGroup(elements, i)
          ? [i, ...captureGroups] : captureGroups;
      iter(~captureGroups, i + 1);
    };

  iter(0);
};
