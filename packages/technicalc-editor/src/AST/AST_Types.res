type rec t =
  /* Arg */
  | @as(0) Arg
  /* Caputure Group */
  | @as(1) CaptureGroupEndS
  /* Atom */
  | @as(2) Acos
  | @as(3) Acosh
  | @as(4) Add
  | @as(5) ArcMinuteUnit
  | @as(6) ArcSecondUnit
  | @as(7) Asin
  | @as(8) Asinh
  | @as(9) Atan
  | @as(10) Atanh
  | @as(11) Bin
  | @as(12) Conj
  | @as(13) DecimalSeparator
  | @as(14) DegreeUnit
  | @as(15) Div
  | @as(16) Dot
  | @as(17) Eq
  | @as(18) Factorial
  | @as(19) Gamma
  | @as(20) GradianUnit
  | @as(21) Gt
  | @as(22) Gte
  | @as(23) Hex
  | @as(24) Im
  | @as(25) Log
  | @as(26) Lt
  | @as(27) Lte
  | @as(28) Mul
  | @as(29) Oct
  | @as(30) OpenBracket
  | @as(31) Percent
  | @as(32) RadianUnit
  | @as(33) Re
  | @as(34) Rref
  | @as(35) Sub
  | @as(36) Trace
  | @as(37) Transpose
  /* AtomS */
  | @as(38) CloseBracketS
  | @as(39) ConstES
  | @as(40) ConstPiS
  | @as(41) CosecS
  | @as(42) CoshS
  | @as(43) CosS
  | @as(44) CotS
  | @as(45) ImaginaryUnitS
  | @as(46) N0_S
  | @as(47) N1_S
  | @as(48) N2_S
  | @as(49) N3_S
  | @as(50) N4_S
  | @as(51) N5_S
  | @as(52) N6_S
  | @as(53) N7_S
  | @as(54) N8_S
  | @as(55) N9_S
  | @as(56) NA_S
  | @as(57) NB_S
  | @as(58) NC_S
  | @as(59) ND_S
  | @as(60) NE_S
  | @as(61) NF_S
  | @as(62) RandS
  | @as(63) SecS
  | @as(64) SinhS
  | @as(65) SinS
  | @as(66) TanhS
  | @as(67) TanS
  | @as(68) XS
  | @as(69) XUnitS
  | @as(70) YS
  | @as(71) YUnitS
  | @as(72) ZS
  | @as(73) ZUnitS
  /* Atom1 */
  | @as(74) Magnitude1
  | @as(75) NLog1
  | @as(76) Superscript1
  /* Atom1S */
  | @as(77) Abs1S
  | @as(78) Ceil1S
  | @as(79) Floor1S
  | @as(80) Round1S
  | @as(81) Sqrt1S
  /* Atom2 */
  | @as(82) Differential2
  | @as(83) NCR2
  | @as(84) NPR2
  | @as(85) Product2
  | @as(86) Sum2
  /* Atom2S */
  | @as(87) Frac2S
  | @as(88) GCD2S
  | @as(89) LCM2S
  | @as(90) Max2S
  | @as(91) Min2S
  | @as(92) NRoot2S
  | @as(93) RandInt2S
  /* Atom3 */
  | @as(94) Integral3
  /* With args */
  | @as(0) CaptureGroupStart({placeholder: option<Symbol.t>})
  | @as(1) EquationArgumentS(int)
  | @as(2) ConstantS({symbol: Symbol.t, value: string})
  | @as(3) VariableS({id: string, symbol: Symbol.t})
  | @as(4) UnitS({prefix: TechniCalcCalculator.Units.prefix, name: TechniCalcCalculator.Units.name})
  | @as(5) TableNS({numRows: int, numColumns: int})
  | @as(6)
  EquationNS({
      symbol: Symbol.t,
      elements: array<t>, // For encoding purposes
      body: TechniCalcCalculator.AST_Types.t,
      arguments: array<option<Symbol.t>>,
    })

let eq = (a: t, b: t) =>
  switch (a, b) {
  | (CaptureGroupStart(a), CaptureGroupStart(b)) =>
    switch (a.placeholder, b.placeholder) {
    | (Some(aSymbol), Some(bSymbol)) => Symbol.eq(aSymbol, bSymbol)
    | (None, None) => true
    | (Some(_), None)
    | (None, Some(_)) => false
    }
  | (ConstantS(a), ConstantS(b)) => Symbol.eq(a.symbol, b.symbol) && a.value == a.value
  | (VariableS(a), VariableS(b)) => a.id == b.id && Symbol.eq(a.symbol, b.symbol)
  | (UnitS(a), UnitS(b)) => a.prefix == b.prefix && a.name == b.name
  | (EquationArgumentS(a), EquationArgumentS(b)) => a == b
  | (TableNS(a), TableNS(b)) => a.numRows == b.numRows && a.numColumns == b.numColumns
  // | (EquationNS(a), EquationNS(b)) => TODO
  | (a, b) => a === b
  }

let argCountExn = (arg: t) =>
  switch arg {
  /* Arg */
  | Arg => assert(false)
  /* Atom0 */
  | CaptureGroupStart(_)
  | CaptureGroupEndS
  | EquationArgumentS(_)
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
  | VariableS(_)
  | UnitS(_) => 0
  /* Atom1 */
  | Magnitude1
  | NLog1
  | Superscript1
  | Abs1S
  | Ceil1S
  | Floor1S
  | Round1S
  | Sqrt1S => 1
  /* Atom2 */
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
  | RandInt2S => 2
  /* Atom3 */
  | Integral3 => 3
  /* Variable */
  | TableNS({numRows, numColumns}) => numRows * numColumns
  | EquationNS({arguments}) => Belt.Array.length(arguments)
  }

type normalizationState =
  | Normalised
  | TooManyArgsError
  | TooFewArgsError(int)

let normalize = (ast: array<t>) => {
  let rec iter = (ast, remaining, i) =>
    switch (remaining, Belt.Array.get(ast, i)) {
    | (0, Some(Arg)) =>
      let ast = Belt.Array.concat(
        Belt.Array.slice(ast, ~offset=0, ~len=i),
        Belt.Array.sliceToEnd(ast, i + 1),
      )
      iter(ast, remaining, i)
    | (_, Some(Arg)) => iter(ast, remaining - 1, i + 1)
    | (_, Some(v)) => iter(ast, remaining + argCountExn(v), i + 1)
    | (0, None) => ast
    | (_, None) =>
      let ast = Belt.Array.concat(ast, Belt.Array.make(remaining, Arg))
      ast
    }

  iter(ast, 0, 0)
}
