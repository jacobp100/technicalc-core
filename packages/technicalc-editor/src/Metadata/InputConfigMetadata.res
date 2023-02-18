type inputSettingsMode = {hasAngleDependentFunction: bool}

let hasAngleDependentFunction = (. element: AST.t) =>
  switch element {
  | Arg
  | CaptureGroupStart(_)
  | CaptureGroupEndS
  | Add
  | Bin
  | Conj
  | DecimalSeparator
  | Div
  | Dot
  | Factorial
  | Gamma
  | Hex
  | Im
  | Log
  | Mul
  | Oct
  | OpenBracket
  | Percent
  | Re
  | Rref
  | Sub
  | Trace
  | Transpose
  | CloseBracketS
  | ConstES
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
  | XUnitS
  | YUnitS
  | ZUnitS
  | ConstantS(_)
  | UnitS(_)
  | VariableS(_)
  | Magnitude1
  | NLog1
  | Superscript1
  | Abs1S
  | Ceil1S
  | Floor1S
  | Round1S
  | Sqrt1S
  | Differential2
  | NCR2
  | NPR2
  | Product2
  | Sum2
  | Frac2S
  | GCD2S
  | LCM2S
  | Max2S
  | Min2S
  | NRoot2S
  | RandInt2S
  | Integral3
  | TableNS(_) => false
  | Acos
  | Acosh
  | ArcMinuteUnit
  | ArcSecondUnit
  | Asin
  | Asinh
  | Atan
  | Atanh
  | DegreeUnit
  | GradianUnit
  | RadianUnit
  | ConstPiS
  | CosecS
  | CoshS
  | CosS
  | CotS
  | SecS
  | SinhS
  | SinS
  | TanhS
  | TanS => true
  }

let inputSettingsMode = (elements: array<AST.t>) => {
  hasAngleDependentFunction: Belt.Array.someU(elements, hasAngleDependentFunction),
}
