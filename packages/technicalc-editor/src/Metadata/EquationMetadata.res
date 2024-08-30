open AST

%%private(
  let argumentIndex = (~argumentsRev: list<option<Symbol.t>>, placeholder: option<Symbol.t>) => {
    let rec iter = (~length, argumentsRev) =>
      switch argumentsRev {
      | list{argument, ...argumentsRev} =>
        let isEq = switch (argument, placeholder) {
        | (Some(argument), Some(placeholder)) => Symbol.eq(argument, placeholder)
        | _ => false
        }
        if isEq {
          (true, Belt.List.length(argumentsRev))
        } else {
          iter(~length=length + 1, argumentsRev)
        }
      | list{} => (false, length)
      }

    iter(~length=0, argumentsRev)
  }
)

let equationMetadata = (elements: array<t>): option<(array<t>, array<option<Symbol.t>>)> => {
  let rec iter = (
    ~openArgumentIndices: list<int>,
    ~equationRev: list<t>,
    ~argumentsRev: list<option<Symbol.t>>,
    i,
  ) => {
    switch Belt.Array.get(elements, i) {
    | Some(CaptureGroupStart({placeholder})) =>
      let (existing, index) = argumentIndex(~argumentsRev, placeholder)
      let openArgumentIndices = list{index, ...openArgumentIndices}
      let argumentsRev = existing ? argumentsRev : list{placeholder, ...argumentsRev}
      iter(~openArgumentIndices, ~equationRev, ~argumentsRev, i + 1)
    | Some(CaptureGroupEndS) =>
      switch openArgumentIndices {
      | list{index, ...openArgumentIndices} =>
        let equationRev = list{EquationArgumentS(index), ...equationRev}
        iter(~openArgumentIndices, ~equationRev, ~argumentsRev, i + 1)
      | list{} => None
      }
    | Some(element) =>
      let equationRev = list{element, ...equationRev}
      iter(~openArgumentIndices, ~equationRev, ~argumentsRev, i + 1)
    | None =>
      let equation = Belt.List.toArray(equationRev)->ArrayUtil.reverseInPlace
      let arguments = Belt.List.toArray(argumentsRev)->ArrayUtil.reverseInPlace
      Some((equation, arguments))
    }
  }

  iter(~openArgumentIndices=list{}, ~equationRev=list{}, ~argumentsRev=list{}, 0)
}
