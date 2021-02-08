open AST_Types;
open AST_OfString;

let ofConstant = (a: TechniCalcCalculator.Real_Constant.t) =>
  switch (a) {
  | Unit => [||]
  | Pi(1) => [|ConstPiS|]
  | Exp(1) => [|ConstES|]
  | Pi(e) =>
    Belt.Array.concatMany([|
      [|ConstPiS, Superscript1|],
      ofString(Belt.Int.toString(e)),
      [|Arg|],
    |])
  | Exp(e) =>
    Belt.Array.concatMany([|
      [|ConstES, Superscript1|],
      ofString(Belt.Int.toString(e)),
      [|Arg|],
    |])
  | Sqrt(s) =>
    Belt.Array.concatMany([|
      [|Sqrt1S|],
      ofString(Belt.Int.toString(s)),
      [|Arg|],
    |])
  };

let ofReal = (a: TechniCalcCalculator.Real.t) =>
  switch (a) {
  | Rational(n, d, c) =>
    let out = Belt.Int.toString(n)->ofString;
    let out = c != Unit ? Belt.Array.concat(out, ofConstant(c)) : out;
    let out =
      if (d != 1) {
        Belt.Array.concatMany([|
          [|Frac2S|],
          out,
          [|Arg|],
          Belt.Int.toString(d)->ofString,
          [|Arg|],
        |]);
      } else {
        out;
      };
    out;
  | Decimal(f) => TechniCalcCalculator.Decimal.toString(f)->ofString
  };

let ofScalar = (a: TechniCalcCalculator.Scalar.t) =>
  switch (a) {
  | `Z => [|N0_S|]
  | `R(re) => ofReal(re)
  | `I(im) => ofReal(im)
  | `C(re, im) =>
    TechniCalcCalculator.(
      Belt.Array.concatMany([|
        ofReal(re),
        Real.(lt(im, zero)) ? [|Sub|] : [||],
        Real.abs(im)->ofReal,
      |])
    )
  };

let ofValue = (a: TechniCalcCalculator.Value.t) =>
  switch (a) {
  | #TechniCalcCalculator.Scalar.t as s => ofScalar(s)
  | `P((`Z | `R(_) | `I(_)) as p) =>
    Belt.Array.concat(ofScalar(p), [|Percent|])
  | `P(p) =>
    Belt.Array.concatMany([|
      [|OpenBracket|],
      ofScalar(p),
      [|CloseBracketS, Percent|],
    |])
  | `V([|a, b|]) =>
    Belt.Array.concatMany([|
      [|Vector2S|],
      ofScalar(a),
      [|Arg|],
      ofScalar(b),
      [|Arg|],
    |])
  | `V([|a, b, c|]) =>
    Belt.Array.concatMany([|
      [|Vector3S|],
      ofScalar(a),
      [|Arg|],
      ofScalar(b),
      [|Arg|],
      ofScalar(c),
      [|Arg|],
    |])
  | `M({numRows: 2, numColumns: 2, elements: [|a, b, c, d|]}) =>
    Belt.Array.concatMany([|
      [|Matrix4S|],
      ofScalar(a),
      [|Arg|],
      ofScalar(b),
      [|Arg|],
      ofScalar(c),
      [|Arg|],
      ofScalar(d),
      [|Arg|],
    |])
  | `M({numRows: 3, numColumns: 3, elements: [|a, b, c, d, e, f, g, h, i|]}) =>
    Belt.Array.concatMany([|
      [|Matrix9S|],
      ofScalar(a),
      [|Arg|],
      ofScalar(b),
      [|Arg|],
      ofScalar(c),
      [|Arg|],
      ofScalar(d),
      [|Arg|],
      ofScalar(e),
      [|Arg|],
      ofScalar(f),
      [|Arg|],
      ofScalar(g),
      [|Arg|],
      ofScalar(h),
      [|Arg|],
      ofScalar(i),
      [|Arg|],
    |])
  | `V(_)
  | `M(_) => [||]
  | `N => [|N0_S, Div, N0_S|]
  };
