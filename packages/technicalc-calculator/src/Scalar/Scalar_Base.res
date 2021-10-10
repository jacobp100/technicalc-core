open Scalar_Types

let zero: t = #Zero
let one: t = #Real(Real.one)
let minusOne: t = #Real(Real.minusOne)
let i: t = #Imag(Real.one)
let minusI: t = #Imag(Real.minusOne)
let pi: t = #Real(Real.pi)
let e: t = #Real(Real.e)
let nan: t = #NaNN

let isNaN = (a: t) =>
  switch a {
  | #NaNN => true
  | _ => false
  }

type realClassification = Zero | NaN | Real

%%private(
  let classifyReal = (re: Real.t) =>
    if Real.eq(re, Real.zero) {
      Zero
    } else if Real.isNaN(re) {
      NaN
    } else {
      Real
    }
)

let ofReal = (a: Real.t): t =>
  switch classifyReal(a) {
  | Zero => zero
  | NaN => nan
  | Real => #Real(a)
  }
let ofImag = (a: Real.t): t =>
  switch classifyReal(a) {
  | Zero => zero
  | NaN => nan
  | Real => #Imag(a)
  }
let ofComplex = (re: Real.t, im: Real.t): t =>
  switch (classifyReal(re), classifyReal(im)) {
  | (Zero, Zero) => zero
  | (NaN, _) | (_, NaN) => nan
  | (Real, Zero) => #Real(re)
  | (Zero, Real) => #Imag(im)
  | (Real, Real) => #Cmpx(re, im)
  }

let ofDecimal = (v): t => ofReal(Real.ofDecimal(v))

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
  | #Zero => Decimal.zero
  | #Real(re) => Real.toDecimal(re)
  | _ => Decimal.nan
  }

let toInt = (a: t): option<int> =>
  switch a {
  | #Zero => Some(0)
  | #Real(re) => Real.toInt(re)
  | _ => None
  }

let toFloat = (a: t): float =>
  switch a {
  | #Zero => 0.
  | #Real(re) => Real.toDecimal(re)->Decimal.toFloat
  | _ => Pervasives.nan
  }

let eq = (a: t, b: t): bool =>
  switch (a, b) {
  | (#Zero, #Zero) => true
  | (#Real(a), #Real(b))
  | (#Imag(a), #Imag(b)) =>
    Real.eq(a, b)
  | (#Cmpx(aRe, aIm), #Cmpx(bRe, bIm)) => Real.eq(aRe, bRe) && Real.eq(aIm, bIm)
  | (#NaNN, #NaNN) => true
  | _ => false
  }

let mapU = (a: t, f: (. Real.t) => Real.t): t =>
  switch a {
  | #Zero => a
  | #Real(re) => ofReal(f(. re))
  | #Imag(im) => ofImag(f(. im))
  | #Cmpx(re, im) => ofComplex(f(. re), f(. im))
  | #NaNN => a
  }
