open Scalar_Types
open Scalar_Base

let neg = mapU(_, (. x) => Real.neg(x))

let add = (a: t, b: t): t =>
  switch (a, b) {
  | (#Zero, _) => b
  | (_, #Zero) => a
  | (#Real(a), #Real(b)) => ofReal(Real.add(a, b))
  | (#Imag(a), #Imag(b)) => ofImag(Real.add(a, b))
  | (#Real(re), #Imag(im))
  | (#Imag(im), #Real(re)) =>
    ofComplex(re, im)
  | (#Real(aRe), #Cmpx(bRe, im))
  | (#Cmpx(bRe, im), #Real(aRe)) =>
    ofComplex(Real.add(aRe, bRe), im)
  | (#Imag(aIm), #Cmpx(re, bIm))
  | (#Cmpx(re, bIm), #Imag(aIm)) =>
    ofComplex(re, Real.add(aIm, bIm))
  | (#Cmpx(aRe, aIm), #Cmpx(bRe, bIm)) => ofComplex(Real.add(aRe, bRe), Real.add(aIm, bIm))
  | (#NaNN, _) | (_, #NaNN) => nan
  }

let sub = (a: t, b: t): t => add(a, neg(b))

let mul = (a: t, b: t): t =>
  switch (a, b) {
  | (#Zero, #Zero)
  | (#Zero, #Real(_) | #Imag(_) | #Cmpx(_))
  | (#Real(_) | #Imag(_) | #Cmpx(_), #Zero) => zero
  | (#Real(a), #Real(b)) => ofReal(Real.mul(a, b))
  | (#Imag(a), #Imag(b)) => ofReal(Real.mul(a, b)->Real.neg)
  | (#Real(re), #Imag(im))
  | (#Imag(im), #Real(re)) =>
    ofImag(Real.mul(re, im))
  | (#Real(aRe), #Cmpx(bRe, bIm))
  | (#Cmpx(bRe, bIm), #Real(aRe)) =>
    let re = Real.mul(aRe, bRe)
    let im = Real.mul(aRe, bIm)
    ofComplex(re, im)
  | (#Imag(aIm), #Cmpx(bRe, bIm))
  | (#Cmpx(bRe, bIm), #Imag(aIm)) =>
    let re = Real.mul(aIm, bIm)
    let im = Real.mul(aIm, bRe)
    ofComplex(Real.neg(re), im)
  | (#Cmpx(aRe, aIm), #Cmpx(bRe, bIm)) =>
    let reRe = Real.mul(aRe, bRe)
    let imIm = Real.mul(aIm, bIm)
    let reIm = Real.mul(aRe, bIm)
    let imRe = Real.mul(aIm, bRe)
    let re = Real.sub(reRe, imIm)
    let im = Real.add(reIm, imRe)
    ofComplex(re, im)
  | (#NaNN, _) | (_, #NaNN) => nan
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
  | (_, #Zero) => ofReal(Real.nan)
  | (#Zero, _) => zero
  | (#Real(a), #Real(b)) => ofReal(Real.div(a, b))
  | (#Imag(im), #Real(re)) => ofImag(Real.div(im, re))
  | (#Real(re), #Imag(im)) => ofImag(Real.div(Real.neg(re), im))
  | (#Imag(a), #Imag(b)) => ofReal(Real.div(a, b))
  | (#Cmpx(aRe, aIm), #Real(bRe)) =>
    let re = Real.div(aRe, bRe)
    let im = Real.div(aIm, bRe)
    ofComplex(re, im)
  | (#Cmpx(aRe, aIm), #Imag(bIm)) =>
    let re = Real.div(aIm, bIm)
    let im = Real.div(Real.neg(aRe), bIm)
    ofComplex(re, im)
  | (#Real(aRe), #Cmpx(bRe, bIm)) =>
    let s = magnitudeSquared(bRe, bIm)
    let re = Real.mul(aRe, bRe)->Real.div(s)
    let im = Real.mul(Real.neg(aRe), bIm)->Real.div(s)
    ofComplex(re, im)
  | (#Imag(aIm), #Cmpx(bRe, bIm)) =>
    let s = magnitudeSquared(bRe, bIm)
    let re = Real.mul(aIm, bIm)->Real.div(s)
    let im = Real.mul(aIm, bRe)->Real.div(s)
    ofComplex(re, im)
  | (#Cmpx(_), #Cmpx(bRe, bIm)) =>
    let s = magnitudeSquared(bRe, bIm)
    let bRecipRe = Real.div(bRe, s)
    let bRecipIm = Real.div(Real.neg(bIm), s)
    let bRecip = #Cmpx(bRecipRe, bRecipIm)
    mul(a, bRecip)
  | (#NaNN, _) | (_, #NaNN) => nan
  }
