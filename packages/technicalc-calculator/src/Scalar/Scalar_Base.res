open Scalar_Types

let zero: t = #R(Real.zero)
let one: t = #R(Real.one)
let minusOne: t = #R(Real.minusOne)
let i: t = #I(Real.one)
let minusI: t = #I(Real.minusOne)
let pi: t = #R(Real.pi)
let e: t = #R(Real.e)
let nan: t = #R(Real.nan)

let normalize = (v: t): t =>
  switch v {
  | #Z
  | #R(Rational(0, _, _))
  | #I(Rational(0, _, _))
  | #C(Rational(0, _, _), Rational(0, _, _)) =>
    #Z
  | #C(Rational(0, _, _), v) => #I(v)
  | #C(v, Rational(0, _, _)) => #R(v)
  | #R(_)
  | #I(_)
  | #C(_) => v
  }

let isNaN = (a: t) =>
  switch a {
  | #Z => false
  | #R(v)
  | #I(v) =>
    Real.isNaN(v)
  | #C(re, im) => Real.isNaN(re) || Real.isNaN(im)
  }

let ofReal = (a: Real.t): t => normalize(#R(a))
let ofImag = (a: Real.t): t => normalize(#I(a))
let ofComplex = (re: Real.t, im: Real.t): t => normalize(#C(re, im))

let ofInt = (v): t =>
  if v == 0 {
    #Z
  } else {
    #R(Real.ofInt(v))
  }

let ofFloat = (v): t =>
  if !FloatUtil.isFinite(v) {
    nan
  } else if v == 0. {
    #Z
  } else {
    let magnitude = 1.e6
    let intMaxF = Belt.Float.fromInt(IntUtil.maxInt)
    let numeratorF = v *. magnitude
    switch (FloatUtil.asInt(numeratorF), FloatUtil.asInt(magnitude)) {
    | (Some(numerator), Some(denominator)) if FloatUtil.abs(numeratorF) < intMaxF =>
      #R(Real.ofRational(numerator, denominator, Unit))->normalize
    | _ => #R(Real.ofDecimal(Decimal.ofFloat(v)))->normalize
    }
  }

let toDecimal = (a: t): Decimal.t =>
  switch a {
  | #Z => Decimal.zero
  | #R(re) => Real.toDecimal(re)
  | _ => Decimal.nan
  }

let toInt = (a: t): option<int> =>
  switch a {
  | #Z => Some(0)
  | #R(re) => Real.toInt(re)
  | _ => None
  }

let toFloat = (a: t): float =>
  switch a {
  | #Z => 0.
  | #R(re) => Real.toDecimal(re)->Decimal.toFloat
  | _ => Pervasives.nan
  }

let eq = (a: t, b: t): bool =>
  switch (a, b) {
  | (#Z, #Z) => true
  | (#R(a), #R(b))
  | (#I(a), #I(b)) =>
    Real.eq(a, b)
  | (#C(aRe, aIm), #C(bRe, bIm)) => Real.eq(aRe, bRe) && Real.eq(aIm, bIm)
  | _ => false
  }

let map = (a: t, f: Real.t => Real.t): t =>
  switch a {
  | #Z => #Z
  | #R(re) => #R(f(re))
  | #I(im) => #I(f(im))
  | #C(re, im) => #C(f(re), f(im))
  }
