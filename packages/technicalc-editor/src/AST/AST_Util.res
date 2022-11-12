open AST_Types

let argEndIndex = (x: array<t>, index) => {
  let rec iter = (~pending, index) => {
    switch Belt.Array.get(x, index) {
    | Some(Arg) =>
      let pending = pending - 1
      if pending <= 0 {
        index + 1
      } else {
        iter(~pending, index + 1)
      }
    | Some(v) => iter(~pending=pending + argCountExn(v), index + 1)
    | None => index
    }
  }
  iter(~pending=0, index)
}
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

let enclosingFunction = (x: array<t>, index) => {
  let rec iter = startIndex =>
    switch Belt.Array.get(x, startIndex) {
    | Some(Arg) => iter(startIndex - 1)
    | Some(e) if argCountExn(e) == 0 => iter(startIndex - 1)
    | Some(element) =>
      let endIndex = argEndIndex(x, startIndex)
      if index >= startIndex && index < endIndex {
        Some((element, startIndex, endIndex))
      } else {
        iter(startIndex - 1)
      }
    | None => None
    }
  iter(index - 1)
}

%%private(
  let rec nextFunctionArgIndex = (x: array<t>, index) => {
    switch Belt.Array.get(x, index) {
    | Some(Arg) => Some(index)
    | _ =>
      switch advanceIndex(x, index) {
      | Some((index, _)) => nextFunctionArgIndex(x, index)
      | None => None
      }
    }
  }
)

let functionArgRanges = (x: array<t>, index) => {
  switch Belt.Array.get(x, index) {
  | Some(Arg)
  | None => []
  | Some(element) =>
    let (_, ranges) = ArrayUtil.foldMakeU(argCountExn(element), index + 1, (. startIndex, _) => {
      let endIndex = nextFunctionArgIndex(x, startIndex)->Belt.Option.getExn
      (endIndex + 1, (startIndex, endIndex))
    })
    ranges
  }
}
