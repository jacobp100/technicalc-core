open Scalar_Types

let zero: t = #Z
let one: t = #R(Real.one)
let minusOne: t = #R(Real.minusOne)
let i: t = #I(Real.one)
let minusI: t = #I(Real.minusOne)
let pi: t = #R(Real.pi)
let e: t = #R(Real.e)
let nan: t = #N

let normalize = (v: t): t =>
  switch v {
  | #Z => zero
  | #R(Rational(0, _, _) as r)
  | #I(Rational(0, _, _) as r) =>
    !Real.isNaN(r) ? zero : nan
  | #C(Rational(0, _, _) as re, Rational(0, _, _) as im) =>
    !Real.isNaN(re) && !Real.isNaN(im) ? zero : nan
  | #C(Rational(0, _, _) as r, v) => !Real.isNaN(r) ? #I(v) : nan
  | #C(v, Rational(0, _, _) as r) => !Real.isNaN(r) ? #R(v) : nan
  | #R(r)
  | #I(r) =>
    !Real.isNaN(r) ? v : nan
  | #C(re, im) => !Real.isNaN(re) && !Real.isNaN(im) ? v : nan
  | #N => v
  }

let isNaN = (a: t) =>
  switch a {
  | #N => true
  | _ => false
  }

let ofReal = (a: Real.t): t => normalize(#R(a))
let ofImag = (a: Real.t): t => normalize(#I(a))
let ofComplex = (re: Real.t, im: Real.t): t => normalize(#C(re, im))

let ofDecimal = (v): t => normalize(#R(Real.ofDecimal(v)))

let ofInt = (v): t => ofReal(Real.ofInt(v))

let ofFloat = (v): t =>
  if !FloatUtil.isFinite(v) {
    nan
  } else if v == 0. {
    zero
  } else {
    let magnitude = 1.e6
    let intMaxF = Belt.Float.fromInt(IntUtil.maxInt)
    let numeratorF = v *. magnitude
    switch (FloatUtil.asInt(numeratorF), FloatUtil.asInt(magnitude)) {
    | (Some(numerator), Some(denominator)) if FloatUtil.abs(numeratorF) < intMaxF =>
      ofReal(Real.ofRational(numerator, denominator, Unit))
    | _ => ofReal(Real.ofDecimal(Decimal.ofFloat(v)))
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
  | (#N, #N) => true
  | _ => false
  }

let map = (a: t, f: Real.t => Real.t): t =>
  switch a {
  | #Z => a
  | #R(re) => ofReal(f(re))
  | #I(im) => ofImag(f(im))
  | #C(re, im) => ofComplex(f(re), f(im))
  | #N => a
  }
