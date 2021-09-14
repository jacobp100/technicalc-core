open Value_Core

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
  | #Z => nan
  | #R(Rational(1 | 2, 1, Unit)) => ofReal(Real.ofInt(1))
  | #R(Rational(3, 1, Unit)) => ofReal(Real.ofInt(2))
  | #R(Rational(4, 1, Unit)) => ofReal(Real.ofInt(6))
  | #R(Rational(5, 1, Unit)) => ofReal(Real.ofInt(24))
  | #R(Rational(6, 1, Unit)) => ofReal(Real.ofInt(120))
  | #R(Rational(7, 1, Unit)) => ofReal(Real.ofInt(720))
  | #R(Rational(8, 1, Unit)) => ofReal(Real.ofInt(5040))
  | #R(Rational(9, 1, Unit)) => ofReal(Real.ofInt(40320))
  | #R(Rational(10, 1, Unit)) => ofReal(Real.ofInt(362880))
  | #R(Rational(11, 1, Unit)) => ofReal(Real.ofInt(3628800))
  | #R(Rational(12, 1, Unit)) => ofReal(Real.ofInt(479001600))
  | #R(re) if Real.gt(re, Real.zero) =>
    open Decimal
    /* See https://github.com/josdejong/mathjs/blob/c5971b371a5610caf37de0d6507a1c7150280f09/src/function/probability/gamma.js */
    let n = Real.toDecimal(re) - half
    let x =
      p->Belt.Array.reduceWithIndexU(p0, (. accum, pi, i) => accum + pi / (n + ofInt(i) + half))
    let t = n + g
    ofDecimal(sqrt2Pi * t ** n * exp(-t) * x)
  | (#R(_) | #I(_) | #C(_)) as xV =>
    open Decimal
    let (nRe, nIm) = switch xV {
    | #R(re) => (Real.toDecimal(re), zero)
    | #I(im) => (zero, Real.toDecimal(im))
    | #C(re, im) => (Real.toDecimal(re), Real.toDecimal(im))
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

    open Value_Core
    let t = n + ofDecimal(g)
    ofDecimal(sqrt2Pi) * t ** n * exp(-t) * x
  | _ => nan
  }

let factorial = x => (x + one)->gamma
