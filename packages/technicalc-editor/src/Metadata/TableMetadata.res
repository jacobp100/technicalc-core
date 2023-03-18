open AST

let tableRanges = (elements: array<AST.t>) => {
  let rec iter = (~accum, i) =>
    switch Belt.Array.get(elements, i) {
    | Some(TableNS(_)) =>
      let accum = switch AST_Util.advanceScopeIndex(elements, i) {
      | Some(i') =>
        let range = (i + 1, i' - 1)
        list{range, ...accum}
      | None => accum
      }
      iter(~accum, i + 1)
    | Some(_) => iter(~accum, i + 1)
    | None => accum
    }

  iter(~accum=Ranges.empty, 0)
}

let insideTable = (tableRanges, index) => Ranges.contains(tableRanges, index)
