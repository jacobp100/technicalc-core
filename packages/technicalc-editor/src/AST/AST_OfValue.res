open AST_Types
open AST_OfString

let ofConstant = (a: TechniCalcCalculator.Real_Constant.t) =>
  switch a {
  | Unit => []
  | Pi(1) => [ConstPiS]
  | Exp(1) => [ConstES]
  | Pi(e) =>
    Belt.Array.concatMany([[ConstPiS, Superscript1], ofString(Belt.Int.toString(e)), [Arg]])
  | Exp(e) =>
    Belt.Array.concatMany([[ConstES, Superscript1], ofString(Belt.Int.toString(e)), [Arg]])
  | Sqrt(s) => Belt.Array.concatMany([[Sqrt1S], ofString(Belt.Int.toString(s)), [Arg]])
  }

let ofReal = (a: TechniCalcCalculator.Real.t) =>
  switch a {
  | Rational(n, d, c) =>
    let out = Belt.Int.toString(n)->ofString
    let out = c != Unit ? Belt.Array.concat(out, ofConstant(c)) : out
    let out = if d != 1 {
      Belt.Array.concatMany([[Frac2S], out, [Arg], Belt.Int.toString(d)->ofString, [Arg]])
    } else {
      out
    }
    out
  | Decimal(f) => TechniCalcCalculator.Decimal.toString(f)->ofString
  }

let ofScalar = (a: TechniCalcCalculator.Scalar.t) =>
  switch a {
  | #Zero => [N0_S]
  | #Real(re) => ofReal(re)
  | #Imag(im) => ofReal(im)
  | #Cmpx(re, im) =>
    open TechniCalcCalculator
    Belt.Array.concatMany([ofReal(re), Real.lt(im, Real.zero) ? [Sub] : [], Real.abs(im)->ofReal])
  | #NaNN => [N0_S, Div, N0_S]
  }

%%private(let ofFinite = x => TechniCalcCalculator.Scalar.Finite.toScalar(x)->ofScalar)

let ofValue = (a: TechniCalcCalculator.Value.t) =>
  switch a {
  | #...TechniCalcCalculator.Scalar.t as s => ofScalar(s)
  | #Pcnt((#Zero | #Real(_) | #Imag(_)) as p) => Belt.Array.concat(ofScalar(p), [Percent])
  | #Pcnt(p) => Belt.Array.concatMany([[OpenBracket], ofFinite(p), [CloseBracketS, Percent]])
  | #Vect([a, b]) => Belt.Array.concatMany([[Vector2S], ofFinite(a), [Arg], ofFinite(b), [Arg]])
  | #Vect([a, b, c]) =>
    Belt.Array.concatMany([[Vector3S], ofFinite(a), [Arg], ofFinite(b), [Arg], ofFinite(c), [Arg]])
  | #Matx({numRows: 2, numColumns: 2, elements: [a, b, c, d]}) =>
    Belt.Array.concatMany([
      [Matrix4S],
      ofFinite(a),
      [Arg],
      ofFinite(b),
      [Arg],
      ofFinite(c),
      [Arg],
      ofFinite(d),
      [Arg],
    ])
  | #Matx({numRows: 3, numColumns: 3, elements: [a, b, c, d, e, f, g, h, i]}) =>
    Belt.Array.concatMany([
      [Matrix9S],
      ofFinite(a),
      [Arg],
      ofFinite(b),
      [Arg],
      ofFinite(c),
      [Arg],
      ofFinite(d),
      [Arg],
      ofFinite(e),
      [Arg],
      ofFinite(f),
      [Arg],
      ofFinite(g),
      [Arg],
      ofFinite(h),
      [Arg],
      ofFinite(i),
      [Arg],
    ])
  | #Vect(_)
  | #Matx(_) => []
  }
