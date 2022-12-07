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
  range: option<(int, int)>,
}

let reduce = (state, element, (i, i') as range) => {
  let range = switch state.range {
  | Some((j, j')) =>
    let i = i < j ? i : j
    let i' = i' > j' ? i' : j'
    Some((i, i'))
  | None => Some(range)
  }

  switch element {
  | Fold_Base(numBase) =>
    switch state {
    | {numBase: None, numString: "", numSup: None, magSup: None} =>
      Some(Ok({...state, range, numBase: Some(numBase)}))
    | _ => Some(Error(Some(i')))
    }
  | Fold_Digit({nucleus, superscript}) =>
    switch state {
    | {numSup: None, magSup: None} if numberIsValidForBase(state.numBase, nucleus) =>
      let numString = state.numString ++ nucleus
      let numSup = Belt.Option.mapU(superscript, superscriptBodyU)
      Some(Ok({...state, range, numString, numSup}))
    | _ => Some(Error(Some(i')))
    }
  | Fold_DecimalSeparator =>
    switch state {
    | {numHasDecimal: false, numSup: None, magSup: None} =>
      Some(Ok({...state, range, numString: state.numString ++ ".", numHasDecimal: true}))
    | _ => Some(Error(Some(i')))
    }
  | Fold_Magnitude({value}) =>
    switch state {
    | {magSup: None} if state.numString != "" => Some(Ok({...state, range, magSup: Some(value)}))
    | _ => Some(Error(Some(i')))
    }
  | _ => None
  }
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
    Some((out, Belt.Option.getExn(state.range)))
  }

let range = state => state.range

let empty = {
  numBase: None,
  numString: "",
  numHasDecimal: false,
  numSup: None,
  magSup: None,
  range: None,
}
