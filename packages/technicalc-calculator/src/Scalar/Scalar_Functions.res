open Scalar_Types
open Scalar_Base

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

let conj = (a: t): t =>
  switch a {
  | #Z
  | #R(_) => a
  | #I(im) => #I(Real.neg(im))
  | #C(re, im) => #C(re, Real.neg(im))
  }

%%private(
  let map2Real = (a: t, b: t, fn: (Real.t, Real.t) => Real.t): t =>
    switch (a, b) {
    | (#R(aRe), #R(bRe)) => #R(fn(aRe, bRe))
    | _ => nan
    }
)

let max = (a: t, b: t): t => map2Real(a, b, Real.max)
let min = (a: t, b: t): t => map2Real(a, b, Real.min)
let gcd = (a: t, b: t): t => map2Real(a, b, Real.gcd)
let lcm = (a: t, b: t): t => map2Real(a, b, Real.lcm)
