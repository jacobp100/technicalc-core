open Scalar_Types
open Scalar_Base
open Scalar_Operators
open Scalar_Exponentiation

%%private(let two = ofInt(2))

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
  | _ => exp(log(a) / two)
  }

let inv = mapU(_, (. x) => Real.inv(x))
let abs = mapU(_, (. x) => Real.abs(x))
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

let max = (a: t, b: t): t => map2Real(a, b, (. a, b) => Real.max(a, b))
let min = (a: t, b: t): t => map2Real(a, b, (. a, b) => Real.min(a, b))
let gcd = (a: t, b: t): t => map2Real(a, b, (. a, b) => Real.gcd(a, b))
let lcm = (a: t, b: t): t => map2Real(a, b, (. a, b) => Real.lcm(a, b))
