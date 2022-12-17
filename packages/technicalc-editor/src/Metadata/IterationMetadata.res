open AST

%%private(
  let bracketRangeEndIndex = (elements, i) => {
    let rec iter = (~bracketLevel, i) =>
      switch advanceScopeIndex(elements, i) {
      | Some(i) =>
        switch Belt.Array.get(elements, i) {
        | Some(CloseBracketS) if bracketLevel == 1 => i
        | Some(OpenBracket) => iter(~bracketLevel=bracketLevel + 1, i)
        | Some(CloseBracketS) => iter(~bracketLevel=bracketLevel - 1, i)
        | _ => iter(~bracketLevel, i)
        }
      | None => i
      }

    iter(~bracketLevel=0, i)
  }
)

%%private(
  let firstOperatorIndex = (elements, i) => {
    let rec iter = (~bracketLevel, i) =>
      switch advanceScopeIndex(elements, i) {
      | Some(i) =>
        switch Belt.Array.get(elements, i) {
        | Some(Add | Sub | Mul | Div | Dot) if bracketLevel == 0 => i
        | Some(OpenBracket) => iter(~bracketLevel=bracketLevel + 1, i)
        | Some(CloseBracketS) => iter(~bracketLevel=bracketLevel - 1, i)
        | _ => iter(~bracketLevel, i)
        }
      | None => i
      }

    iter(~bracketLevel=0, i)
  }
)

%%private(
  let operatorRange = (elements, i) =>
    switch advanceScopeIndex(elements, i) {
    | Some(r) =>
      let r' = switch Belt.Array.get(elements, r) {
      | Some(OpenBracket) => bracketRangeEndIndex(elements, i)
      | _ => firstOperatorIndex(elements, i)
      }
      Some((r, r'))
    | None => None
    }
)

let iterationRanges = (elements: array<AST.t>) => {
  let rec iter = (~accum, i) =>
    switch Belt.Array.get(elements, i) {
    | Some(Sum2 | Product2) =>
      let accum = switch operatorRange(elements, i) {
      | Some(range) => list{range, ...accum}
      | None => accum
      }
      iter(~accum, i + 1)
    | Some(Differential2) =>
      let (r, r') = functionArgRangesExn(elements, i)->Belt.Array.getExn(0)
      let range = (r, r' - 1)
      iter(~accum=list{range, ...accum}, i + 1)
    | Some(Integral3) =>
      let (r, r') = functionArgRangesExn(elements, i)->Belt.Array.getExn(2)
      let range = (r, r' - 1)
      iter(~accum=list{range, ...accum}, i + 1)
    | Some(_) => iter(~accum, i + 1)
    | None => accum
    }

  iter(~accum=Ranges.empty, 0)
}

let insideIterator = (iterationRanges, index) => Ranges.contains(iterationRanges, index)
