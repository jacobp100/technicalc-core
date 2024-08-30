type rec t =
  /* Arg */
  | Arg
  /* Caputure Group */
  | CaptureGroupStart({placeholder: option<Symbol.t>})
  | CaptureGroupEndS
  /* Equation */
  | EquationArgumentS(int)
  /* Atom */
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
  /* AtomS */
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
  | ConstantS({symbol: Symbol.t, value: string})
  | UnitS({prefix: TechniCalcCalculator.Units.prefix, name: TechniCalcCalculator.Units.name})
  | VariableS({id: string, symbol: Symbol.t})
  /* Atom1 */
  | Magnitude1
  | NLog1
  | Superscript1
  /* Atom1S */
  | Abs1S
  | Ceil1S
  | Floor1S
  | Round1S
  | Sqrt1S
  /* Atom2 */
  | Differential2
  | NCR2
  | NPR2
  | Product2
  | Sum2
  /* Atom2S */
  | Frac2S
  | GCD2S
  | LCM2S
  | Max2S
  | Min2S
  | NRoot2S
  | RandInt2S
  /* Atom3 */
  | Integral3
  /* Variable */
  | TableNS({numRows: int, numColumns: int})
  | EquationNS({
      symbol: Symbol.t,
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
  | Arg => assert false
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
