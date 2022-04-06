type captureGroup = {
  index: int,
  placeholderMml: option<string>,
  elements: array<AST.t>,
}

let captureGroups = (elements: array<AST.t>): array<captureGroup> => {
  let rec iter = (~captureGroupStartStack=list{}, ~captureGroupsRev=list{}, index) =>
    switch Belt.Array.get(elements, index) {
    | Some(CaptureGroupStart({placeholderMml})) =>
      let captureGroupStartStack = list{(index + 1, placeholderMml), ...captureGroupStartStack}
      iter(~captureGroupStartStack, ~captureGroupsRev, index + 1)
    | Some(CaptureGroupEndS) =>
      switch captureGroupStartStack {
      | list{(start, placeholderMml), ...captureGroupStartStack} =>
        let elements =
          Belt.Array.slice(elements, ~offset=start, ~len=index - start)->AST_Types.normalize

        let captureGroup = {
          index: start,
          placeholderMml: placeholderMml,
          elements: elements,
        }
        let captureGroupsRev = list{captureGroup, ...captureGroupsRev}

        iter(~captureGroupStartStack, ~captureGroupsRev, index + 1)
      | list{} => iter(~captureGroupStartStack, ~captureGroupsRev, index + 1)
      }
    | Some(_) => iter(~captureGroupStartStack, ~captureGroupsRev, index + 1)
    | None => Belt.List.toArray(captureGroupsRev)->ArrayUtil.reverseInPlace
    }

  iter(0)
}
