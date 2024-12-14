open AST_Types

type hint =
  | @as(0) Value({symbol: Symbol.t, value: string})
  | @as(1) Variable({id: string})
  | @as(2) CaptureGroup({placeholder: Symbol.t, isEmpty: bool})

%%private(
  let captureGroupHintAtIndex = (elements: array<t>, index: int) => {
    // Can ignore Args and brackets here
    let rec iter = (~argumentIndex, index) =>
      switch Belt.Array.get(elements, index) {
      | Some(CaptureGroupEndS) => None
      | Some(CaptureGroupStart({placeholder: Some(placeholder)})) =>
        let isEmpty = switch Belt.Array.get(elements, index + 1) {
        | Some(CaptureGroupEndS) => true
        | _ => false
        }
        Some(CaptureGroup({placeholder, isEmpty}))
      | Some(EquationNS({arguments})) =>
        switch (
          Belt.Array.getUnsafe(arguments, argumentIndex),
          AST_Util.functionArgRangesExn(elements, index)->Belt.Array.get(argumentIndex),
        ) {
        | (Some(placeholder), Some((r0, r1))) =>
          let isEmpty = r0 == r1 - 1
          Some(CaptureGroup({placeholder, isEmpty}))
        | _ => None
        }
      | Some(Arg) => iter(~argumentIndex=argumentIndex + 1, index - 1)
      | Some(e) =>
        let argumentIndex = argumentIndex - argCountExn(e)
        let argumentIndex = max(argumentIndex, 0)
        iter(~argumentIndex, index - 1)
      | None => None
      }

    iter(~argumentIndex=0, index - 1)
  }
)

%%private(
  let elementHintAtIndex = (elements: array<t>, index: int) =>
    switch Belt.Array.get(elements, index) {
    | Some(ConstantS({symbol, value})) => Some(Value({symbol, value}))
    | Some(VariableS({id})) => Some(Variable({id: id}))
    | _ => None
    }
)

let hint = (elements: array<t>, index: int): option<hint> =>
  // Prefer previous element
  switch elementHintAtIndex(elements, index - 1) {
  | Some(_) as hint => hint
  | None =>
    switch elementHintAtIndex(elements, index) {
    | Some(_) as hint => hint
    | None => captureGroupHintAtIndex(elements, index)
    }
  }
