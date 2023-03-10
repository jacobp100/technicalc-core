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
    | DegreeUnit
    | Div
    | Dot
    | Factorial
    | Gamma
    | GradianUnit
    | Hex
    | Im
    | Log
    | Mul
    | Oct
    | OpenBracket
    | Percent
    | RadianUnit
    | Re
    | Rref
    | Sub
    | Trace
    | Transpose
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
    | ConstantS(_)
    | UnitS(_)
    | VariableS(_)
    | XUnitS
    | YUnitS
    | ZUnitS
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
    | Integral3
    | TableNS(_) => true
    }
)

%%private(
  let placeholderIndicesListRev = (elements: array<t>) => {
    let rec iter = (~indicesRev, ~functionStack, i) =>
      switch Belt.Array.get(elements, i) {
      | Some(Arg) =>
        let (indicesRev, functionStack) = switch functionStack {
        | list{(index, argCount), ...functionStack} =>
          let lastFunctionWasEmpty = index == i - 1
          let indicesRev = lastFunctionWasEmpty ? list{i, ...indicesRev} : indicesRev
          let nextArgCount = argCount - 1
          let functionStack =
            nextArgCount != 0 ? list{(i, nextArgCount), ...functionStack} : functionStack
          (indicesRev, functionStack)
        | list{} => (indicesRev, functionStack)
        }
        iter(~indicesRev, ~functionStack, i + 1)
      | Some(Superscript1) =>
        let needsPlaceholder = switch advanceScopeIndex(~direction=Backwards, elements, i - 1) {
        | Some(index) =>
          let fn = Belt.Array.getExn(elements, index + 1)
          !acceptsSuperscript(fn)
        | None => true
        }
        let indicesRev = needsPlaceholder ? list{i, ...indicesRev} : indicesRev
        let functionStack = list{(i, 1), ...functionStack}
        iter(~indicesRev, ~functionStack, i + 1)
      | Some(element) =>
        let argCount = argCountExn(element)
        let functionStack = argCount != 0 ? list{(i, argCount), ...functionStack} : functionStack
        iter(~indicesRev, ~functionStack, i + 1)
      | None => indicesRev
      }

    iter(~indicesRev=list{}, ~functionStack=list{}, 0)
  }
)

let reifyPlaceholders = (elements: array<t>) => {
  let rec iter = (~indicesRev, elements) =>
    switch indicesRev {
    | list{index, ...indicesRev} =>
      let insertedElements = [CaptureGroupStart({placeholder: None}), CaptureGroupEndS]
      let elements = ArrayUtil.insertArray(elements, insertedElements, index)
      iter(~indicesRev, elements)
    | list{} => elements
    }
  iter(~indicesRev=placeholderIndicesListRev(elements), elements)
}
