open AST_Types
open AST_OfString

%%private(
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
)

%%private(
  let ofReal = (a: TechniCalcCalculator.Real.t) =>
    switch a {
    | Rational(n, d, c) =>
      let out = Belt.Int.toString(n)
      let (negative, out) = if StringUtil.startsWith(out, "-") {
        (true, StringUtil.sliceToEnd(out, 1))
      } else {
        (false, out)
      }
      let out = ofString(out)
      let out = c != Unit ? Belt.Array.concat(out, ofConstant(c)) : out
      let out = if d != 1 {
        Belt.Array.concatMany([[Frac2S], out, [Arg], Belt.Int.toString(d)->ofString, [Arg]])
      } else {
        out
      }
      if negative {
        Belt.Array.concat([Sub], out)
      } else {
        out
      }
    | Decimal(f) => TechniCalcCalculator.Decimal.toString(f)->ofString
    }
)

%%private(
  let ofScalar = (a: TechniCalcCalculator.Scalar.t) =>
    switch a {
    | #Zero => [N0_S]
    | #Real(re) => ofReal(re)
    | #Imag(im) => ofReal(im)
    | #Cmpx(re, im) =>
      let imLtZero = TechniCalcCalculator.Decimal.lt(
        TechniCalcCalculator.Real_Base.toDecimal(im),
        TechniCalcCalculator.Decimal.zero,
      )
      Belt.Array.concatMany([ofReal(re), imLtZero ? [] : [Add], ofReal(im)])
    | #NaNN => [N0_S, Div, N0_S]
    }
)

%%private(let ofFinite = x => TechniCalcCalculator.Scalar.Finite.toScalar(x)->ofScalar)

%%private(
  let ofUnitU = (. {prefix, name, power}: TechniCalcCalculator.Units.t) => {
    let unit = UnitS({prefix, name})
    if power == 1 {
      [unit]
    } else {
      let power = Belt.Int.toString(power)->ofString
      Belt.Array.concatMany([[unit, Superscript1], power, [Arg]])
    }
  }
)

let ofValue = (a: TechniCalcCalculator.Value.t) =>
  switch a {
  | #...TechniCalcCalculator.Scalar.t as s => ofScalar(s)
  | #Pcnt((#Zero | #Real(_) | #Imag(_)) as p) => Belt.Array.concat(ofScalar(p), [Percent])
  | #Pcnt(p) => Belt.Array.concatMany([[OpenBracket], ofFinite(p), [CloseBracketS, Percent]])
  | #Vect(elements) =>
    Belt.Array.reduceU(
      elements,
      [TableNS({numRows: Belt.Array.length(elements), numColumns: 1})],
      (. accum, element) => Belt.Array.concatMany([accum, ofFinite(element), [Arg]]),
    )
  | #Matx({numRows, numColumns, elements}) =>
    Belt.Array.reduceU(elements, [TableNS({numRows, numColumns})], (. accum, element) => {
      Belt.Array.concatMany([accum, ofFinite(element), [Arg]])
    })
  | #Mesr({value, units}) => Belt.Array.concat(ofReal(value), Belt.Array.flatMapU(units, ofUnitU))
  }
