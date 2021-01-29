type t =
  /* Arg */
  | Arg
  /* Caputure Group */
  | CaptureGroupStart({placeholderMml: string})
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
  | DegreeFunction
  | DegreeUnit
  | Div
  | Dot
  | Factorial
  | Gamma
  | GradianFunction
  | GradianUnit
  | Hex
  | Im
  | Log
  | Mul
  | Oct
  | OpenBracket
  | Percent
  | Re
  | Sub
  | UnitConversion({
      fromUnits: array(TechniCalcCalculator.Unit_Types.unitPart),
      toUnits: array(TechniCalcCalculator.Unit_Types.unitPart),
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
  | CustomAtomS({
      mml: string,
      value: string,
    })
  | VariableS(string)
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
  | Gcd2S
  | Lcm2S
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
  | Matrix9S;

let eq = (a: t, b: t) =>
  switch (a, b) {
  | (
      CaptureGroupStart({placeholderMml: a1}),
      CaptureGroupStart({placeholderMml: b1}),
    ) =>
    a1 == b1
  | (UnitConversion(_), UnitConversion(_)) => false // Not used yet (ignore)
  | (CustomAtomS({mml: a1, value: a2}), CustomAtomS({mml: b1, value: b2})) =>
    a1 == b1 && a2 == b2
  | (VariableS(a1), VariableS(b1)) => a1 == b1
  | (a, b) => a === b
  };

let argCountExn = (arg: t) =>
  switch (arg) {
  /* Arg */
  | Arg => assert(false)
  /* Caputure Group Start */
  | CaptureGroupStart(_)
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
  | DegreeFunction
  | DegreeUnit
  | Div
  | Dot
  | Factorial
  | Gamma
  | GradianFunction
  | GradianUnit
  | Hex
  | Im
  | Log
  | Mul
  | Oct
  | OpenBracket
  | Percent
  | Re
  | Sub
  | UnitConversion(_)
  /* AtomS */
  | CloseBracketS
  | ConstES
  | ConstPiS
  | CosecS
  | CoshS
  | CosS
  | CotS
  | CustomAtomS(_)
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
  | VariableS(_) => 0
  /* Atom1 */
  | Magnitude1
  | NLog1
  | Superscript1
  /* Atom1S */
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
  /* Atom2S */
  | Frac2S
  | Gcd2S
  | Lcm2S
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
  };

let argEndIndex = (ast: array(t), index) => {
  let rec iter = (~pending, index) =>
    switch (Belt.Array.get(ast, index)) {
    | Some(Arg) =>
      if (pending == 0) {
        index + 1;
      } else {
        iter(~pending=pending - 1, index + 1);
      }
    | Some(v) => iter(~pending=pending + argCountExn(v), index + 1)
    | None => index
    };
  iter(~pending=0, index);
};

let rec normalizationState = (ast, remaining, i) =>
  switch (remaining, Belt.Array.get(ast, i)) {
  | (0, Some(Arg)) => `GenericError
  | (_, Some(Arg)) => normalizationState(ast, remaining - 1, i + 1)
  | (_, Some(v)) =>
    normalizationState(ast, remaining + argCountExn(v), i + 1)
  | (0, None) => `Ok
  | (_, None) => `TooFewArgsError(remaining)
  };

let normalize = (ast: array(t)) =>
  switch (normalizationState(ast, 0, 0)) {
  | `Ok => ast
  | `GenericError =>
    let remaining = ref(0);
    let ast =
      Belt.Array.keep(ast, element =>
        if (element != Arg) {
          remaining := remaining^ + argCountExn(element);
          true;
        } else if (remaining^ != 0) {
          remaining := remaining^ - 1;
          true;
        } else {
          false;
        }
      );
    if (remaining^ != 0) {
      Belt.Array.concat(ast, Belt.Array.make(remaining^, Arg));
    } else {
      ast;
    };
  | `TooFewArgsError(remaining) =>
    Belt.Array.concat(ast, Belt.Array.make(remaining, Arg))
  };
