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

let inv = map(_, Real.inv)
let abs = map(_, Real.abs)
let round = map(_, Real.round)
let floor = map(_, Real.floor)
let ceil = map(_, Real.ceil)

let ofDeg = map(_, Real.ofDeg)
let ofArcMin = map(_, Real.ofArcMin)
let ofArcSec = map(_, Real.ofArcSec)
let ofGrad = map(_, Real.ofGrad)

let toDeg = map(_, Real.toDeg)
let toArcMinute = map(_, Real.toArcMinute)
let toArcSecond = map(_, Real.toArcSecond)
let toGrad = map(_, Real.toGrad)

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
  let map2Real = (a: t, b: t, fn: (Real.t, Real.t) => Real.t): t =>
    switch (a, b) {
    | (#Real(aRe), #Real(bRe)) => ofReal(fn(aRe, bRe))
    | _ => nan
    }
)

let max = (a: t, b: t): t => map2Real(a, b, Real.max)
let min = (a: t, b: t): t => map2Real(a, b, Real.min)
let gcd = (a: t, b: t): t => map2Real(a, b, Real.gcd)
let lcm = (a: t, b: t): t => map2Real(a, b, Real.lcm)
