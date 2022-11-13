open EditState_Base
open EditState_Types
open EditState_Util

let setIndex = ({elements, formatCaptureGroups}, index) => {
  make(~index, ~elements, ~formatCaptureGroups)
}

%%private(
  let moveIndexInDirection = (~forwards, {index, elements, formatCaptureGroups}) => {
    let length = Belt.Array.length(elements)
    let step = forwards ? 1 : -1

    let rec iter = index => {
      let preferredShiftDirection = EditState_Util.preferredShiftDirection(
        ~index,
        ~elements,
        ~formatCaptureGroups,
      )
      let nextIndex = switch preferredShiftDirection {
      | Some(_) => Some(index + step)
      | None => None
      }
      switch nextIndex {
      | Some(nextIndex) if nextIndex >= 0 && nextIndex <= length => iter(nextIndex)
      | _ =>
        let index = preferredInsertionIndex(~index, ~elements, ~formatCaptureGroups)
        {index, elements, formatCaptureGroups}
      }
    }

    iter(index + step)
  }
)

let previous = s => moveIndexInDirection(~forwards=false, s)

let next = s => moveIndexInDirection(~forwards=true, s)

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
    switch AST_Util.enclosingFunction(elements, index) {
    | Some((fn, startIndex, _)) =>
      switch deltaFactor(~fn) {
      | Some(deltaFactor) =>
        let ranges = AST_Util.functionArgRanges(elements, startIndex)
        switch Belt.Array.getIndexByU(ranges, (. (start, end)) => index >= start && index <= end) {
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
  | Some((_, nextEnd)) => nextEnd
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
