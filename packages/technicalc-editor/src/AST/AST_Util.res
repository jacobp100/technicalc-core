open AST_Types

type direction = Forwards | Backwards

let advanceIndex = (~direction=Forwards, x, startIndex) => {
  let step = direction == Forwards ? 1 : -1
  let argLevelStep = step

  let rec iter = (~argLevel, index) =>
    switch Belt.Array.get(x, index) {
    | None => None
    | Some(v) =>
      let index = index + step
      let argLevel = switch v {
      | Arg => argLevel - argLevelStep
      | _ => argLevel + argLevelStep * argCountExn(v)
      }

      if argLevel == 0 {
        let fn = direction == Forwards ? Belt.Array.getExn(x, startIndex) : v
        Some((index, fn))
      } else if argLevel > 0 {
        iter(~argLevel, index)
      } else {
        None
      }
    }
  iter(~argLevel=0, startIndex)
}

let bracketLevel = (~direction=Forwards, ~from=0, x: array<t>) => {
  let rec iter = (~bracketLevel, index) =>
    switch Belt.Array.get(x, index) {
    | None => bracketLevel
    | Some(v) =>
      let bracketLevel = switch v {
      | OpenBracket => bracketLevel + 1
      | CloseBracketS => bracketLevel - 1
      | _ => bracketLevel
      }
      switch advanceIndex(x, index, ~direction) {
      | Some((index, _)) => iter(~bracketLevel, index)
      | None => bracketLevel
      }
    }
  iter(~bracketLevel=0, from)
}