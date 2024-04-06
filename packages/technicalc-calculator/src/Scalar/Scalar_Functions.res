open Scalar_Types
open Scalar_Base
open Scalar_Arithmetic

let exp = (a: t): t =>
  switch a {
  | #Zero => one
  | #Real(re) => ofReal(Real.exp(re))
  | #Imag(im) =>
    let re = Real_Trig.cos(im)
    let im = Real_Trig.sin(im)
    ofComplex(re, im)
  | #Cmpx(re, im) =>
    let exp = Real.exp(re)
    let re = Real_Trig.cos(im)->Real.mul(exp)
    let im = Real_Trig.sin(im)->Real.mul(exp)
    ofComplex(re, im)
  | #NaNN => nan
  }

%%private(
  @inline
  let arg = a => {
    open Real

    switch a {
    | #Real(re) => re >= zero ? zero : pi
    | #Imag(im) =>
      let sign = im >= zero ? 1 : -1
      ofRational(sign, 2, Pi(1))
    | #Cmpx(re, im) =>
      let reF = toDecimal(re)
      let imF = toDecimal(im)
      ofDecimal(Decimal.atan2(imF, reF))
    }
  }
)

let log = (a: t): t =>
  switch a {
  | #Zero => nan
  | #Real(gtZero) if Real.gt(gtZero, Real.zero) => ofReal(Real.logExn(gtZero))
  | #Real(Rational(-1, 1, Unit)) => mul(pi, i)
  | (#Real(_) | #Imag(_) | #Cmpx(_)) as vV =>
    let re = switch vV {
    | #Real(re) => Real.mul(re, re)
    | #Imag(im) => Real.mul(im, im)
    | #Cmpx(re, im) => Real.add(Real.mul(re, re), Real.mul(im, im))
    }
    let re = Real.div(Real.logExn(re), Real.ofInt(2))
    let im = arg(vV)
    ofComplex(re, im)
  | #NaNN => nan
  }

let logBase = (~base: t, a: t) =>
  switch (base, a) {
  | (#Real(Rational(_, _, Unit) as base), #Real(Rational(_, _, Unit) as a))
    if Real.gt(a, Real.zero) =>
    open Real
    let real = logExn(a) / logExn(base)

    // Attempt to recover from precision loss
    let decimal = toDecimal(real)
    let real = switch Real_DecimalUtil.roundedRationalOfDecimal(~denimonator=60, decimal) {
    | Some(rational) if pow(base, rational) == a => rational
    | _ => real
    }

    ofReal(real)
  | _ => div(log(a), log(base))
  }

let sqrt = (a: t) =>
  switch a {
  | #Zero => zero
  | #Real(re) =>
    open Real
    if gte(re, zero) {
      sqrt(re)->ofReal
    } else {
      abs(re)->sqrt->ofImag
    }
  | _ => div(log(a), ofInt(2))->exp
  }

// Should be in Scalar_Arithmetic - but relies upon sqrt
let pow = (a: t, b: t): t =>
  switch (a, b) {
  | (#Zero, #Zero) => nan
  | (#Zero, #Real(_) | #Imag(_) | #Cmpx(_)) => zero
  | (#Real(_) | #Imag(_) | #Cmpx(_), #Zero) => one
  | (#Real(Rational(1, 1, Exp(1))), _) => exp(b)
  | (_, #Real(Rational(1, 2, Unit))) => sqrt(a)
  | (_, #Real(Rational(2, 1, Unit))) => mul(a, a)
  | (#Real(a), #Real(b)) if Real.gte(a, Real.zero) || Real.toInt(b) != None =>
    ofReal(Real.pow(a, b))
  | (#Imag(im), #Real(Rational(bInt, 1, Unit))) =>
    let aPowB = Real.powInt(im, bInt)
    switch IntUtil.safeMod(bInt, 4) {
    | Some(0) => ofReal(aPowB)
    | Some(1) => ofImag(aPowB)
    | Some(2) => ofReal(Real.neg(aPowB))
    | Some(3) => ofImag(Real.neg(aPowB))
    | _ => assert false
    }
  | (#Real(_) | #Imag(_) | #Cmpx(_), #Real(_) | #Imag(_) | #Cmpx(_)) => mul(log(a), b)->exp
  | (#NaNN, _) | (_, #NaNN) => nan
  }

let abs = (x: t): t =>
  switch x {
  | #Zero => #Zero
  | #Real(re) => #Real(Real.abs(re))
  | #Imag(im) => #Real(Real.abs(im))
  | #Cmpx(re, im) => #Real(Real.sqrt(Real.add(Real.mul(re, re), Real.mul(im, im))))
  | #NaNN => nan
  }

let inv = (a: t) =>
  switch a {
  | #Real(re) => #Real(Real.inv(re))
  | _ => div(one, a)
  }

let round = mapU(_, (. x) => Real.round(x))
let floor = mapU(_, (. x) => Real.floor(x))
let ceil = mapU(_, (. x) => Real.ceil(x))

let ofDeg = mapU(_, (. x) => Real.ofDeg(x))
let ofArcMin = mapU(_, (. x) => Real.ofArcMin(x))
let ofArcSec = mapU(_, (. x) => Real.ofArcSec(x))
let ofGrad = mapU(_, (. x) => Real.ofGrad(x))

let toDeg = mapU(_, (. x) => Real.toDeg(x))
let toArcMinute = mapU(_, (. x) => Real.toArcMinute(x))
let toArcSecond = mapU(_, (. x) => Real.toArcSecond(x))
let toGrad = mapU(_, (. x) => Real.toGrad(x))

let re = (a: t): t =>
  switch a {
  | #Zero
  | #Imag(_) => zero
  | #Real(_) => a
  | #Cmpx(re, _) => ofReal(re)
  | #NaNN => nan
  }

let im = (a: t): t =>
  switch a {
  | #Zero
  | #Real(_) => zero
  | #Imag(_) => a
  | #Cmpx(_, im) => ofReal(im)
  | #NaNN => nan
  }

let conj = (a: t): t =>
  switch a {
  | #Zero
  | #Real(_) => a
  | #Imag(im) => ofImag(Real.neg(im))
  | #Cmpx(re, im) => ofComplex(re, Real.neg(im))
  | #NaNN => nan
  }

%%private(
  let map2Real = (a: t, b: t, fn: (. Real.t, Real.t) => Real.t): t =>
    switch (a, b) {
    | (#Real(aRe), #Real(bRe)) => ofReal(fn(. aRe, bRe))
    | _ => nan
    }
)

let rem = (a: t, b: t): t => map2Real(a, b, (. a, b) => Real.rem(a, b))
let max = (a: t, b: t): t => map2Real(a, b, (. a, b) => Real.max(a, b))
let min = (a: t, b: t): t => map2Real(a, b, (. a, b) => Real.min(a, b))
let gcd = (a: t, b: t): t => map2Real(a, b, (. a, b) => Real.gcd(a, b))
let lcm = (a: t, b: t): t => map2Real(a, b, (. a, b) => Real.lcm(a, b))
