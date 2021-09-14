open Scalar_Types
open Scalar_Base

let neg = map(_, Real.neg)

let add = (a: t, b: t): t =>
  switch (a, b) {
  | (#Z, _) => b
  | (_, #Z) => a
  | (#R(a), #R(b)) => ofReal(Real.add(a, b))
  | (#I(a), #I(b)) => ofImag(Real.add(a, b))
  | (#R(re), #I(im))
  | (#I(im), #R(re)) =>
    ofComplex(re, im)
  | (#R(aRe), #C(bRe, im))
  | (#C(bRe, im), #R(aRe)) =>
    ofComplex(Real.add(aRe, bRe), im)
  | (#I(aIm), #C(re, bIm))
  | (#C(re, bIm), #I(aIm)) =>
    ofComplex(re, Real.add(aIm, bIm))
  | (#C(aRe, aIm), #C(bRe, bIm)) => ofComplex(Real.add(aRe, bRe), Real.add(aIm, bIm))
  | (#N, _) | (_, #N) => nan
  }

let sub = (a: t, b: t): t => add(a, neg(b))

let mul = (a: t, b: t): t =>
  switch (a, b) {
  | (#Z, #Z)
  | (#Z, #R(_) | #I(_) | #C(_))
  | (#R(_) | #I(_) | #C(_), #Z) => zero
  | (#R(a), #R(b)) => ofReal(Real.mul(a, b))
  | (#I(a), #I(b)) => ofReal(Real.mul(a, b)->Real.neg)
  | (#R(re), #I(im))
  | (#I(im), #R(re)) =>
    ofImag(Real.mul(re, im))
  | (#R(aRe), #C(bRe, bIm))
  | (#C(bRe, bIm), #R(aRe)) =>
    let re = Real.mul(aRe, bRe)
    let im = Real.mul(aRe, bIm)
    ofComplex(re, im)
  | (#I(aIm), #C(bRe, bIm))
  | (#C(bRe, bIm), #I(aIm)) =>
    let re = Real.mul(aIm, bIm)
    let im = Real.mul(aIm, bRe)
    ofComplex(Real.neg(re), im)
  | (#C(aRe, aIm), #C(bRe, bIm)) =>
    let reRe = Real.mul(aRe, bRe)
    let imIm = Real.mul(aIm, bIm)
    let reIm = Real.mul(aRe, bIm)
    let imRe = Real.mul(aIm, bRe)
    let re = Real.sub(reRe, imIm)
    let im = Real.add(reIm, imRe)
    ofComplex(re, im)
  | (#N, _) | (_, #N) => nan
  }

%%private(
  let magnitudeSquared = (re, im) => {
    let re = Real.mul(re, re)
    let im = Real.mul(im, im)
    Real.add(re, im)
  }
)

let div = (a: t, b: t): t =>
  switch (a, b) {
  | (_, #Z) => ofReal(Real.nan)
  | (#Z, _) => zero
  | (#R(a), #R(b)) => ofReal(Real.div(a, b))
  | (#I(im), #R(re)) => ofImag(Real.div(im, re))
  | (#R(re), #I(im)) => ofImag(Real.div(Real.neg(re), im))
  | (#I(a), #I(b)) => ofReal(Real.div(a, b))
  | (#C(aRe, aIm), #R(bRe)) =>
    let re = Real.div(aRe, bRe)
    let im = Real.div(aIm, bRe)
    ofComplex(re, im)
  | (#C(aRe, aIm), #I(bIm)) =>
    let re = Real.div(aIm, bIm)
    let im = Real.div(Real.neg(aRe), bIm)
    ofComplex(re, im)
  | (#R(aRe), #C(bRe, bIm)) =>
    let s = magnitudeSquared(bRe, bIm)
    let re = Real.mul(aRe, bRe)->Real.div(s)
    let im = Real.mul(Real.neg(aRe), bIm)->Real.div(s)
    ofComplex(re, im)
  | (#I(aIm), #C(bRe, bIm)) =>
    let s = magnitudeSquared(bRe, bIm)
    let re = Real.mul(aIm, bIm)->Real.div(s)
    let im = Real.mul(aIm, bRe)->Real.div(s)
    ofComplex(re, im)
  | (#C(_), #C(bRe, bIm)) =>
    let s = magnitudeSquared(bRe, bIm)
    let bRecipRe = Real.div(bRe, s)
    let bRecipIm = Real.div(Real.neg(bIm), s)
    let bRecip = #C(bRecipRe, bRecipIm)
    mul(a, bRecip)
  | (#N, _) | (_, #N) => nan
  }
