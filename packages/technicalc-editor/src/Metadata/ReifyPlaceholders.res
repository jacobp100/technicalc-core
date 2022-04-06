open AST

%%private(
  let acceptsSuperscript = (element: t) =>
    switch element {
    | Arg
    | CaptureGroupStart(_)
    | Acos
    | Acosh
    | Add
    | ArcMinuteUnit
    | ArcSecondUnit
    | Asin
    | Asinh
    | Atan
    | Atanh
    | Bin
    | Conj
    | DecimalSeparator
    | DegFunction
    | DegreeUnit
    | Div
    | Dot
    | Factorial
    | Gamma
    | GradFunction
    | GradianUnit
    | Hex
    | Im
    | Log
    | Mul
    | Oct
    | OpenBracket
    | Percent
    | RadFunction
    | RadianUnit
    | Re
    | Rem
    | Sub
    | UnitConversion(_)
    | Magnitude1
    | NLog1
    | Superscript1
    | Differential2
    | NCR2
    | NPR2
    | Product2
    | Sum2 => false

    | CaptureGroupEndS
    | CloseBracketS
    | ConstES
    | ConstPiS
    | CosecS
    | CoshS
    | CosS
    | CotS
    | ImaginaryUnitS
    | IterationXS
    | N0_S
    | N1_S
    | N2_S
    | N3_S
    | N4_S
    | N5_S
    | N6_S
    | N7_S
    | N8_S
    | N9_S
    | NA_S
    | NB_S
    | NC_S
    | ND_S
    | NE_S
    | NF_S
    | RandS
    | SecS
    | SinhS
    | SinS
    | TanhS
    | TanS
    | CustomAtomS(_)
    | VariableS(_)
    | Abs1S
    | Ceil1S
    | Floor1S
    | Round1S
    | Sqrt1S
    | Frac2S
    | GCD2S
    | LCM2S
    | Max2S
    | Min2S
    | NRoot2S
    | RandInt2S
    | Vector2S
    | Integral3
    | MFrac3S
    | Vector3S
    | Matrix4S
    | Matrix9S => true
    }
)

%%private(
  let placeholderIndicesListRev = (elements: array<t>) => {
    let rec iter = (~indicesRev, ~lastWasFunctionOrArg, i) =>
      switch Belt.Array.get(elements, i) {
      | Some(Arg) if lastWasFunctionOrArg =>
        let indicesRev = list{i, ...indicesRev}
        iter(~indicesRev, ~lastWasFunctionOrArg=true, i + 1)
      | Some(Arg) => iter(~indicesRev, ~lastWasFunctionOrArg=true, i + 1)
      | Some(Superscript1) =>
        let needsPlaceholder = switch advanceIndex(~direction=Backwards, elements, i - 1) {
        | Some((_, element)) if !acceptsSuperscript(element) => true
        | None => true
        | Some(_) => false
        }
        let indicesRev = needsPlaceholder ? list{i, ...indicesRev} : indicesRev
        iter(~indicesRev, ~lastWasFunctionOrArg=true, i + 1)
      | Some(element) =>
        let lastWasFunctionOrArg = argCountExn(element) != 0
        iter(~indicesRev, ~lastWasFunctionOrArg, i + 1)
      | None => indicesRev
      }

    iter(~indicesRev=list{}, ~lastWasFunctionOrArg=false, 0)
  }
)

let reifyPlaceholders = (elements: array<t>) => {
  let rec iter = (~indicesRev, elements) =>
    switch indicesRev {
    | list{index, ...indicesRev} =>
      let insertedElements = [CaptureGroupStart({placeholderMml: None}), CaptureGroupEndS]
      let elements = ArrayUtil.insertArray(elements, insertedElements, index)
      iter(~indicesRev, elements)
    | list{} => elements
    }
  iter(~indicesRev=placeholderIndicesListRev(elements), elements)
}
