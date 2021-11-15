open AST
open Value_Types

module Node = TechniCalcCalculator.AST_Types

let numberIsValidForBase = (base, nucleus) =>
  switch (base, nucleus) {
  | (_, "0" | "1")
  | (None | Some(Base_Oct | Base_Hex), "2" | "3" | "4" | "5" | "6" | "7")
  | (None | Some(Base_Hex), "8" | "9")
  | (Some(Base_Hex), "A" | "B" | "C" | "D" | "E" | "F") => true
  | _ => false
  }

type numState = {
  numBase: option<base>,
  numString: string,
  numHasDecimal: bool,
  numSup: option<node>,
  magSup: option<node>,
}

let reduce = (state, element) =>
  switch (state, element) {
  | ({numBase: None, numString: "", numSup: None, magSup: None}, Fold_Base(numBase)) =>
    Some({...state, numBase: Some(numBase)})
  | ({numSup: None, magSup: None}, Fold_Digit({nucleus, superscript}))
    if numberIsValidForBase(state.numBase, nucleus) =>
    Some({
      ...state,
      numString: state.numString ++ nucleus,
      numSup: Belt.Option.map(superscript, superscriptBody),
    })
  | ({numHasDecimal: false, numSup: None, magSup: None}, Fold_DecimalSeparator) =>
    Some({...state, numString: state.numString ++ ".", numHasDecimal: true})
  | ({magSup: None}, Fold_Magnitude({value})) if state.numString != "" =>
    Some({...state, magSup: Some(value)})
  | _ => None
  }

let toNode = state =>
  switch state {
  | {numString: "" | "."} => None
  | {numBase, numString, numSup, magSup} =>
    let base = switch numBase {
    | Some(Base_Bin) => 2
    | Some(Base_Oct) => 8
    | Some(Base_Hex) => 16
    | None => 10
    }
    let out = Node.OfStringBase(base, numString)
    let out = numSup->Belt.Option.mapWithDefaultU(out, (. x) => Pow(out, x))
    let out =
      magSup
      ->Belt.Option.mapU((. x) => Node.Pow(OfInt(10), x))
      ->Belt.Option.mapWithDefaultU(out, (. x) => Mul(out, x))
    Some(out)
  }

let empty = {
  numBase: None,
  numString: "",
  numHasDecimal: false,
  numSup: None,
  magSup: None,
}
