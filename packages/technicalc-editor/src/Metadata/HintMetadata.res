open AST_Types

type hint =
  | Value({symbol: Symbol.t, value: string})
  | Variable({id: string})
  | CaptureGroup({placeholder: Symbol.t})

%%private(
  let captureGroupHintAtIndex = (elements: array<t>, index: int) => {
    // Can ignore Args and brackets here
    let rec iter = index =>
      switch Belt.Array.get(elements, index) {
      | Some(CaptureGroupEndS) => None
      | Some(CaptureGroupStart({placeholder})) =>
        let isEmpty = switch Belt.Array.get(elements, index + 1) {
        | Some(CaptureGroupEndS) => true
        | _ => false
        }
        switch placeholder {
        | Some(placeholder) if !isEmpty => Some(CaptureGroup({placeholder: placeholder}))
        | _ => None
        }
      | Some(_) => iter(index - 1)
      | None => None
      }

    iter(index - 1)
  }
)

%%private(
  let elementHintAtIndex = (elements: array<t>, index: int) =>
    switch Belt.Array.get(elements, index) {
    | Some(CustomAtomS({symbol, value})) => Some(Value({symbol, value}))
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
