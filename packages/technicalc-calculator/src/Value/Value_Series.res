open Value_Types
open Value_Base
open Value_Arithmetic
open Value_Functions
open Value_Operators

%%private(let g = Decimal.ofFloat(4.7421875))

%%private(let p0 = Decimal.ofFloat(0.99999999999999709182))

%%private(
  let p = [
    Decimal.ofFloat(57.156235665862923517),
    Decimal.ofFloat(-59.597960355475491248),
    Decimal.ofFloat(14.136097974741747174),
    Decimal.ofFloat(-0.49191381609762019978),
    Decimal.ofFloat(0.33994649984811888699e-4),
    Decimal.ofFloat(0.46523628927048575665e-4),
    Decimal.ofFloat(-0.98374475304879564677e-4),
    Decimal.ofFloat(0.15808870322491248884e-3),
    Decimal.ofFloat(-0.21026444172410488319e-3),
    Decimal.ofFloat(0.21743961811521264320e-3),
    Decimal.ofFloat(-0.16431810653676389022e-3),
    Decimal.ofFloat(0.84418223983852743293e-4),
    Decimal.ofFloat(-0.26190838401581408670e-4),
    Decimal.ofFloat(0.36899182659531622704e-5),
  ]
)

%%private(let half = Decimal.ofFloat(0.5))
%%private(let sqrt2Pi = Decimal.ofFloat(2.506628274631000502415765284811))

let gamma = (x: t): t =>
  switch x {
  | #Zero => nan
  | #Real(Rational(1 | 2, 1, Unit)) => ofReal(Real.ofInt(1))
  | #Real(Rational(3, 1, Unit)) => ofReal(Real.ofInt(2))
  | #Real(Rational(4, 1, Unit)) => ofReal(Real.ofInt(6))
  | #Real(Rational(5, 1, Unit)) => ofReal(Real.ofInt(24))
  | #Real(Rational(6, 1, Unit)) => ofReal(Real.ofInt(120))
  | #Real(Rational(7, 1, Unit)) => ofReal(Real.ofInt(720))
  | #Real(Rational(8, 1, Unit)) => ofReal(Real.ofInt(5040))
  | #Real(Rational(9, 1, Unit)) => ofReal(Real.ofInt(40320))
  | #Real(Rational(10, 1, Unit)) => ofReal(Real.ofInt(362880))
  | #Real(Rational(11, 1, Unit)) => ofReal(Real.ofInt(3628800))
  | #Real(Rational(12, 1, Unit)) => ofReal(Real.ofInt(479001600))
  | #Real(re) if Real.gt(re, Real.zero) =>
    open Decimal
    /* See https://github.com/josdejong/mathjs/blob/c5971b371a5610caf37de0d6507a1c7150280f09/src/function/probability/gamma.js */
    let n = Real.toDecimal(re) - half
    let x =
      p->Belt.Array.reduceWithIndexU(p0, (. accum, pi, i) => accum + pi / (n + ofInt(i) + half))
    let t = n + g
    ofDecimal(sqrt2Pi * t ** n * exp(-t) * x)
  | (#Real(_) | #Imag(_) | #Cmpx(_)) as xV =>
    let (n, x) = {
      open Decimal
      let (nRe, nIm) = switch xV {
      | #Real(re) => (Real.toDecimal(re), zero)
      | #Imag(im) => (zero, Real.toDecimal(im))
      | #Cmpx(re, im) => (Real.toDecimal(re), Real.toDecimal(im))
      }
      let nRe = nRe - half
      let n = ofComplex(Real.ofDecimal(nRe), Real.ofDecimal(nIm))
      let (xRe, xIm) = p->Belt.Array.reduceWithIndexU((p0, zero), (. (re, im), p, i) => {
        let real = nRe + ofInt(i) + half
        let den = real * real + nIm * nIm
        if den != zero {
          (re + p * real / den, im - p * nIm / den)
        } else {
          (nan, im)
        }
      })
      let x = ofComplex(Real.ofDecimal(xRe), Real.ofDecimal(xIm))

      (n, x)
    }

    let t = n + ofDecimal(g)
    ofDecimal(sqrt2Pi) * t ** n * exp(-t) * x
  | _ => nan
  }

let factorial = x => (x + one)->gamma

%%private(
  let reduceRangeU = (a, b, initialValue, f, iteratee) =>
    switch (toInt(a), toInt(b)) {
    | (Some(a), Some(b)) if b >= a =>
      let current = ref(initialValue)
      for i in a to b {
        current := iteratee(. current.contents, f(. ofInt(i)))
      }
      current.contents
    | _ => nan
    }
)

let sumU = (f, a, b) => reduceRangeU(a, b, zero, f, (. a, b) => add(a, b))
let productU = (f, a, b) => reduceRangeU(a, b, one, f, (. a, b) => mul(a, b))

%%private(
  let factorialIsFinite = x =>
    switch toInt(x) {
    | Some(i) if i < 0 => false
    | _ => true
    }
)

let nPr = (n, r) =>
  if factorialIsFinite(n - r) {
    n->factorial / (n - r)->factorial
  } else {
    zero
  }

let nCr = (n, r) =>
  if factorialIsFinite(r) && factorialIsFinite(n - r) {
    n->factorial / (r->factorial * (n - r)->factorial)
  } else {
    zero
  }
