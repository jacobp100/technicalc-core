type t =
  /* Arg */
  | Arg
  /* Caputure Group */
  | CaptureGroupStart({placeholderMml: option<string>})
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
  | Sub
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
  | CustomAtomS({mml: string, value: string})
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
  | Vector2S
  /* Atom3 */
  | Integral3
  /* Atom3S */
  | MFrac3S
  | Vector3S
  /* Matrices */
  | Matrix4S
  | Matrix9S

let eq = (a: t, b: t) =>
  switch (a, b) {
  | (CaptureGroupStart({placeholderMml: a1}), CaptureGroupStart({placeholderMml: b1})) => a1 == b1
  | (UnitConversion(_), UnitConversion(_)) => false // Not used yet (ignore)
  | (CustomAtomS({mml: a1, value: a2}), CustomAtomS({mml: b1, value: b2})) => a1 == b1 && a2 == b2
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
  | Sub
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
  | RandInt2S
  | Vector2S => 2
  /* Atom3 */
  | Integral3
  /* Atom3S */
  | MFrac3S
  | Vector3S => 3
  /* Matrices */
  | Matrix4S => 4
  | Matrix9S => 9
  }

let argEndIndex = (ast: array<t>, index) => {
  let rec iter = (~pending, index) =>
    switch Belt.Array.get(ast, index) {
    | Some(Arg) =>
      if pending == 0 {
        index + 1
      } else {
        iter(~pending=pending - 1, index + 1)
      }
    | Some(v) => iter(~pending=pending + argCountExn(v), index + 1)
    | None => index
    }
  iter(~pending=0, index)
}

type normalizationState =
  | Normalised
  | GenericError
  | TooFewArgsError(int)

%%private(
  let rec normalizationState = (ast, remaining, i) =>
    switch (remaining, Belt.Array.get(ast, i)) {
    | (0, Some(Arg)) => GenericError
    | (_, Some(Arg)) => normalizationState(ast, remaining - 1, i + 1)
    | (_, Some(v)) => normalizationState(ast, remaining + argCountExn(v), i + 1)
    | (0, None) => Normalised
    | (_, None) => TooFewArgsError(remaining)
    }
)

let normalize = (ast: array<t>) =>
  switch normalizationState(ast, 0, 0) {
  | Normalised => ast
  | GenericError =>
    let remaining = ref(0)
    let ast = Belt.Array.keepU(ast, (. element) => {
      if element != Arg {
        remaining := remaining.contents + argCountExn(element)
        true
      } else if remaining.contents != 0 {
        remaining := remaining.contents - 1
        true
      } else {
        false
      }
    })
    if remaining.contents != 0 {
      Belt.Array.concat(ast, Belt.Array.make(remaining.contents, Arg))
    } else {
      ast
    }
  | TooFewArgsError(remaining) => Belt.Array.concat(ast, Belt.Array.make(remaining, Arg))
  }
