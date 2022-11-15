open EditState_Base
open EditState_Types
open EditState_Util

let setIndex = ({elements, formatCaptureGroups}, index) =>
  make(~index, ~elements, ~formatCaptureGroups)

%%private(
  let moveIndexInDirection = (elements, index, formatCaptureGroups, step) => {
    let length = Belt.Array.length(elements)

    let rec iter = index => {
      let nextIndex = switch preferredShiftDirection(~index, ~elements, ~formatCaptureGroups) {
      | Some(_) => Some(index + step)
      | None => None
      }
      switch nextIndex {
      | Some(nextIndex) if nextIndex >= 0 && nextIndex <= length => iter(nextIndex)
      | _ => index
      }
    }

    iter(index + step)
  }
)

let previous = ({elements, index, formatCaptureGroups}) => {
  let index = moveIndexInDirection(elements, index, formatCaptureGroups, -1)
  make(~elements, ~index, ~formatCaptureGroups)
}

let next = ({elements, index, formatCaptureGroups}) => {
  let index = moveIndexInDirection(elements, index, formatCaptureGroups, 1)
  make(~elements, ~index, ~formatCaptureGroups)
}

%%private(
  @inline
  let deltaFactor = (~fn: AST.t) =>
    switch fn {
    | Frac2S | Differential2 | NRoot2S | MFrac3S => Some(1)
    | Sum2 | Product2 | Integral3 => Some(-1)
    | TableNS({numColumns}) => Some(numColumns)
    | _ => None
    }
)

%%private(
  let moveDelta = (elements: array<AST.t>, index, delta) =>
    switch AST.closestParentFunction(elements, index) {
    | Some((fn, startIndex)) =>
      switch deltaFactor(~fn) {
      | Some(deltaFactor) =>
        let ranges = AST.functionArgRangesExn(elements, startIndex)
        switch Belt.Array.getIndexByU(ranges, (. (start, end)) => index >= start && index < end) {
        | Some(rangeIndex) => Belt.Array.get(ranges, rangeIndex + delta * deltaFactor)
        | None => None
        }
      | None => None
      }
    | None => None
    }
)

let moveUp = ({index, elements, formatCaptureGroups}) => {
  let index = switch moveDelta(elements, index, -1) {
  | Some((_, nextEnd)) => nextEnd - 1
  | None => index
  }

  make(~index, ~elements, ~formatCaptureGroups)
}

let moveDown = ({index, elements, formatCaptureGroups}) => {
  let index = switch moveDelta(elements, index, 1) {
  | Some((previousStart, _)) => previousStart
  | None => index
  }

  make(~index, ~elements, ~formatCaptureGroups)
}

let moveStart = ({elements, formatCaptureGroups}) => make(~index=0, ~elements, ~formatCaptureGroups)

let moveEnd = ({elements, formatCaptureGroups}) =>
  make(~index=Belt.Array.length(elements), ~elements, ~formatCaptureGroups)
