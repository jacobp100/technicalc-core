open AST_Types

type direction = | @as(0) Forwards | @as(1) Backwards

/*
The next index within the same scope level - i.e. keeping the parent function
the same. If the element at the index is a function with arguments, it will
jump over it - returning the index after that function's last Arg. Returns None
if the index is outside the bounds of the function.
*/
let advanceScopeIndex = (~direction=Forwards, x, index) => {
  let step = direction == Forwards ? 1 : -1
  let argCountStep = step

  let rec iter = (~pendingArgs, index) =>
    switch Belt.Array.get(x, index) {
    | None => None
    | Some(e) =>
      let index = index + step
      let pendingArgs = switch e {
      | Arg => pendingArgs - argCountStep
      | _ => pendingArgs + argCountStep * argCountExn(e)
      }

      if pendingArgs == 0 {
        Some(index)
      } else if pendingArgs > 0 {
        iter(~pendingArgs, index)
      } else {
        None
      }
    }
  iter(~pendingArgs=0, index)
}

/*
Closest parent function the index is strictly within. Returns the index of the
function element. Since the index of the function element is not within the
function itself, you can recursively call this to traverse through the parents.
*/
let closestParentFunction = (x: array<t>, index) => {
  let rec iter = (~pendingArgs, index) =>
    switch Belt.Array.get(x, index) {
    | Some(Arg) => iter(~pendingArgs=pendingArgs + 1, index - 1)
    | Some(e) =>
      let pendingArgs = pendingArgs - argCountExn(e)
      if pendingArgs < 0 {
        Some((e, index))
      } else {
        iter(~pendingArgs, index - 1)
      }
    | None => None
    }
  iter(~pendingArgs=0, index - 1)
}

/*
Returns an array of the ranges of an element's function arguments. Ranges are
inclusive of the Arg element - except for non-normalized ASTs that are missing
Arg elements. Returns an empty array if the element does not accept arguments.
Throws if the element at the passed index is Arg - or the index is outside the
range of the array.
*/
let functionArgRangesExn = (x: array<t>, index) => {
  let argCount = Belt.Array.getExn(x, index)->argCountExn
  let length = Belt.Array.length(x)
  let out = Belt.Array.make(argCount, (length, length))

  let rec iter = (~current, ~pendingArgs, ~rangeStartIndex, index) =>
    if current < argCount {
      switch Belt.Array.get(x, index) {
      | Some(Arg) =>
        if pendingArgs == 0 {
          let rangeEndIndex = index + 1
          Belt.Array.setExn(out, current, (rangeStartIndex, rangeEndIndex))
          iter(~current=current + 1, ~pendingArgs, ~rangeStartIndex=rangeEndIndex, rangeEndIndex)
        } else {
          iter(~current, ~pendingArgs=pendingArgs - 1, ~rangeStartIndex, index + 1)
        }
      | Some(e) =>
        iter(~current, ~pendingArgs=pendingArgs + argCountExn(e), ~rangeStartIndex, index + 1)
      | None => out
      }
    } else {
      out
    }
  iter(~current=0, ~pendingArgs=0, ~rangeStartIndex=index + 1, index + 1)
}
