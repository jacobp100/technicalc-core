type t =
  /* Arg */
  | Arg
  /* Caputure Group */
  | CaptureGroupStart({placeholder: option<Symbol.t>})
  | CaptureGroupEndS
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
  | Rref
  | Sub
  | Trace
  | Transpose
  | UnitConversion({
      fromUnits: array<TechniCalcCalculator.Unit_Types.t>,
      toUnits: array<TechniCalcCalculator.Unit_Types.t>,
    })
  /* AtomS */
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
  | CustomAtomS({symbol: Symbol.t, value: string})
  | VariableS({id: string, name: string})
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
  /* Table */
  | TableNS({numRows: int, numColumns: int})

let eq = (a: t, b: t) =>
  switch (a, b) {
  | (CaptureGroupStart({placeholder: a1}), CaptureGroupStart({placeholder: b1})) => a1 == b1
  | (UnitConversion(_), UnitConversion(_)) => false // Not used yet (ignore)
  | (CustomAtomS({symbol: a1, value: a2}), CustomAtomS({symbol: b1, value: b2})) =>
    Symbol.eq(a1, b1) && a2 == b2
  | (VariableS({id: a1, name: a2}), VariableS({id: b1, name: b2})) => a1 == b1 && a2 == b2
  | (a, b) => a === b
  }

let argCountExn = (arg: t) =>
  switch arg {
  /* Arg */
  | Arg => assert false
  /* Caputure Group Start */
  | CaptureGroupStart(_)
  | CaptureGroupEndS
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
  | Rref
  | Sub
  | Trace
  | Transpose
  | UnitConversion(_)
  | CloseBracketS
  | ConstES
  | ConstPiS
  | CosecS
  | CoshS
  | CosS
  | CotS
  | CustomAtomS(_)
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
  | VariableS(_) => 0
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
  /* Matrices */
  | TableNS({numRows, numColumns}) => numRows * numColumns
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
