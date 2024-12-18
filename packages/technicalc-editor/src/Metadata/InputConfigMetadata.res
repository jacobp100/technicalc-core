type inputSettingsMode = {hasAngleDependentFunction: bool}

let hasAngleDependentFunction = (element: AST.t) =>
  switch element {
  | Arg
  | CaptureGroupStart(_)
  | CaptureGroupEndS
  | EquationArgumentS(_)
  | Add
  | Bin
  | Conj
  | DecimalSeparator
  | Div
  | Dot
  | Eq
  | Factorial
  | Gamma
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
  | Re
  | Rref
  | Sub
  | Trace
  | Transpose
  | CloseBracketS
  | ConstES
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
  | XS
  | XUnitS
  | YS
  | YUnitS
  | ZS
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
  | TableNS(_)
  | EquationNS(_) => false
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
  hasAngleDependentFunction: Belt.Array.some(elements, hasAngleDependentFunction),
}
