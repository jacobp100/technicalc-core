open Scalar_Types
open Scalar_Base
open Scalar_Arithmetic

%%private(
  let arg = (re, im) =>
    if !Real.eq(re, Real.zero) {
      let reF = Real.toDecimal(re)
      let imF = Real.toDecimal(im)
      Real.ofDecimal(Decimal.atan2(imF, reF))
    } else if Real.gt(im, Real.zero) {
      Real.ofRational(1, 2, Pi(1))
    } else if Real.lt(im, Real.zero) {
      Real.ofRational(-1, 2, Pi(1))
    } else {
      Real.nan
    }
)

let expReal = re =>
  switch re {
  | Real.Rational(0, 1, Unit) => Real.one
  | Rational(i, 1, Unit) => Real.ofRational(1, 1, Exp(i))
  | _ => Real.ofDecimal(Real.toDecimal(re)->Decimal.exp)
  }

let exp = (a: t): t =>
  switch a {
  | #Zero => one
  | #Real(re) => ofReal(expReal(re))
  | #Imag(im) =>
    let re = Real_Trig.cos(im)
    let im = Real_Trig.sin(im)
    ofComplex(re, im)
  | #Cmpx(re, im) =>
    let exp = expReal(re)
    let re = Real_Trig.cos(im)->Real.mul(exp)
    let im = Real_Trig.sin(im)->Real.mul(exp)
    ofComplex(re, im)
  | #NaNN => nan
  }

%%private(
  let logReal = q =>
    switch q {
    | Real.Rational(1, 1, Exp(reExp)) => Real.ofRational(reExp, 1, Unit)
    | _ =>
      let f = Real.toDecimal(q)
      if Decimal.gt(f, Decimal.zero) {
        Real.ofDecimal(Decimal.ln(f))
      } else {
        assert false
      }
    }
)

let log = (a: t): t =>
  switch a {
  | #Zero => nan
  | #Real(gtZero) if Real.gt(gtZero, Real.zero) => ofReal(logReal(gtZero))
  | #Real(Rational(-1, 1, Unit)) => mul(pi, i)
  | (#Real(_) | #Imag(_) | #Cmpx(_)) as vV =>
    let re = switch vV {
    | #Real(re) => Real.mul(re, re)
    | #Imag(im) => Real.mul(im, im)
    | #Cmpx(re, im) => Real.add(Real.mul(re, re), Real.mul(im, im))
    }
    let re = {
      open Real
      div(logReal(re), ofRational(2, 1, Unit))
    }
    let im = switch vV {
    | #Real(re) => arg(re, Real.zero)
    | #Imag(im) => arg(Real.zero, im)
    | #Cmpx(re, im) => arg(re, im)
    }
    ofComplex(re, im)
  | #NaNN => nan
  }
