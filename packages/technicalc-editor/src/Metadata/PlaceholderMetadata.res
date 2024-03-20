open AST

type placeholder =
  | Implicit({index: int})
  | Explicit({index: int, symbol: option<Symbol.t>, elements: array<AST.t>})

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
    | Eq
    | Factorial
    | Gamma
    | GradianUnit
    | Gt
    | Gte
    | Hex
    | Im
    | Log
    | Lt
    | Lte
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
    | XS
    | XUnitS
    | YS
    | YUnitS
    | ZS
    | ZUnitS
    | ConstantS(_)
    | UnitS(_)
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
    | Integral3
    | TableNS(_) => true
    }
)

let placeholders = (elements: array<AST.t>): array<placeholder> => {
  let rec iter = (
    ~functionStack=list{},
    ~captureGroupStartStack=list{},
    ~placeholdersRev=list{},
    index,
  ) =>
    switch Belt.Array.get(elements, index) {
    | Some(CaptureGroupStart({placeholder: symbol})) =>
      let captureGroupStartStack = list{(index + 1, symbol), ...captureGroupStartStack}
      iter(~captureGroupStartStack, ~placeholdersRev, index + 1)
    | Some(CaptureGroupEndS) =>
      switch captureGroupStartStack {
      | list{(start, symbol), ...captureGroupStartStack} =>
        let elements =
          Belt.Array.slice(elements, ~offset=start, ~len=index - start)->AST_Types.normalize

        let placeholder = Explicit({index: start, symbol, elements})
        let placeholdersRev = list{placeholder, ...placeholdersRev}

        iter(~functionStack, ~captureGroupStartStack, ~placeholdersRev, index + 1)
      | list{} => iter(~functionStack, ~captureGroupStartStack, ~placeholdersRev, index + 1)
      }
    | Some(Arg) =>
      let (placeholdersRev, functionStack) = switch functionStack {
      | list{(lastFunctionIndex, needsPopulating, argCount), ...functionStack} =>
        let lastFunctionWasEmpty = lastFunctionIndex == index - 1
        let placeholdersRev = if lastFunctionWasEmpty && needsPopulating {
          let placeholder = Implicit({index: index})
          list{placeholder, ...placeholdersRev}
        } else {
          placeholdersRev
        }
        let functionStack = switch argCount - 1 {
        | 0 => functionStack
        | nextArgCount => list{(index, needsPopulating, nextArgCount), ...functionStack}
        }
        (placeholdersRev, functionStack)
      | list{} => (placeholdersRev, functionStack)
      }
      iter(~functionStack, ~captureGroupStartStack, ~placeholdersRev, index + 1)
    | Some(Superscript1) =>
      let needsPlaceholder = switch advanceScopeIndex(~direction=Backwards, elements, index - 1) {
      | Some(index) =>
        let fn = Belt.Array.getExn(elements, index + 1)
        !acceptsSuperscript(fn)
      | None => true
      }
      let placeholder = Implicit({index: index})
      let placeholdersRev = needsPlaceholder
        ? list{placeholder, ...placeholdersRev}
        : placeholdersRev
      let functionStack = list{(index, true, 1), ...functionStack}
      iter(~functionStack, ~captureGroupStartStack, ~placeholdersRev, index + 1)
    | Some(element) =>
      let needsPopulating = switch element {
      | TableNS(_) => false
      | _ => true
      }
      let functionStack = switch argCountExn(element) {
      | 0 => functionStack
      | argCount => list{(index, needsPopulating, argCount), ...functionStack}
      }
      iter(~functionStack, ~captureGroupStartStack, ~placeholdersRev, index + 1)
    | None => Belt.List.toArray(placeholdersRev)->ArrayUtil.reverseInPlace
    }

  iter(0)
}
