open Scalar_Types;

let add = (a: t, b: t): t =>
  switch (a, b) {
  | (`Z, _) => b
  | (_, `Z) => a
  | (`R(a), `R(b)) => `R(Real.add(a, b))
  | (`I(a), `I(b)) => `I(Real.add(a, b))
  | (`R(re), `I(im))
  | (`I(im), `R(re)) => `C((re, im))
  | (`R(aRe), `C(bRe, im))
  | (`C(bRe, im), `R(aRe)) => `C((Real.add(aRe, bRe), im))
  | (`I(aIm), `C(re, bIm))
  | (`C(re, bIm), `I(aIm)) => `C((re, Real.add(aIm, bIm)))
  | (`C(aRe, aIm), `C(bRe, bIm)) =>
    `C((Real.add(aRe, bRe), Real.add(aIm, bIm)))
  };

let sub = (a: t, b: t): t => add(a, Scalar_Functions.neg(b));

let mul = (a: t, b: t): t =>
  switch (a, b) {
  | (`Z, `Z)
  | (`Z, `R(_) | `I(_) | `C(_))
  | (`R(_) | `I(_) | `C(_), `Z) => `Z
  | (`R(a), `R(b)) => `R(Real.mul(a, b))
  | (`I(a), `I(b)) => `R(Real.mul(a, b)->Real.neg)
  | (`R(re), `I(im))
  | (`I(im), `R(re)) => `I(Real.mul(re, im))
  | (`R(aRe), `C(bRe, bIm))
  | (`C(bRe, bIm), `R(aRe)) =>
    let re = Real.mul(aRe, bRe);
    let im = Real.mul(aRe, bIm);
    `C((re, im));
  | (`I(aIm), `C(bRe, bIm))
  | (`C(bRe, bIm), `I(aIm)) =>
    let re = Real.mul(aIm, bIm);
    let im = Real.mul(aIm, bRe);
    `C((Real.neg(re), im));
  | (`C(aRe, aIm), `C(bRe, bIm)) =>
    let reRe = Real.mul(aRe, bRe);
    let imIm = Real.mul(aIm, bIm);
    let reIm = Real.mul(aRe, bIm);
    let imRe = Real.mul(aIm, bRe);
    let re = Real.sub(reRe, imIm);
    let im = Real.add(reIm, imRe);
    `C((re, im));
  };

let%private magnitudeSquared = (re, im) => {
  let re = Real.mul(re, re);
  let im = Real.mul(im, im);
  Real.add(re, im);
};

let div = (a: t, b: t): t =>
  switch (a, b) {
  | (_, `Z) => `R(Real.nan)
  | (`Z, _) => `Z
  | (`R(a), `R(b)) => `R(Real.div(a, b))
  | (`I(im), `R(re)) => `I(Real.div(im, re))
  | (`R(re), `I(im)) => `I(Real.div(Real.neg(re), im))
  | (`I(a), `I(b)) => `R(Real.div(a, b))
  | (`C(aRe, aIm), `R(bRe)) =>
    let re = Real.div(aRe, bRe);
    let im = Real.div(aIm, bRe);
    `C((re, im));
  | (`C(aRe, aIm), `I(bIm)) =>
    let re = Real.div(aIm, bIm);
    let im = Real.div(Real.neg(aRe), bIm);
    `C((re, im));
  | (`R(aRe), `C(bRe, bIm)) =>
    let s = magnitudeSquared(bRe, bIm);
    let re = Real.mul(aRe, bRe)->Real.div(s);
    let im = Real.mul(Real.neg(aRe), bIm)->Real.div(s);
    `C((re, im));
  | (`I(aIm), `C(bRe, bIm)) =>
    let s = magnitudeSquared(bRe, bIm);
    let re = Real.mul(aIm, bIm)->Real.div(s);
    let im = Real.mul(aIm, bRe)->Real.div(s);
    `C((re, im));
  | (`C(_), `C(bRe, bIm)) =>
    let s = magnitudeSquared(bRe, bIm);
    let bRecipRe = Real.div(bRe, s);
    let bRecipIm = Real.div(Real.neg(bIm), s);
    let bRecip = `C((bRecipRe, bRecipIm));
    mul(a, bRecip);
  };
