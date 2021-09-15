open Real_Types
open Real_Base
open Real_Util

/* Disable ops that can overflow */
// let (+) = None;
// let (-) = None;
// let ( * ) = None;
// let (~-) = None;

%%private(
  let safeRat = (n, d, c) =>
    switch (SafeInt.toInt(n), SafeInt.toInt(d)) {
    | (Some(n), Some(d)) => Some(ofRational(n, d, c))
    | _ => None
    }
)

let inv = a =>
  switch a {
  | Rational(_, 0, _) => nan
  | Rational(n, d, Unit) => ofRational(d, n, Unit)
  | Rational(n, d, Pi(exp) as c) =>
    switch SafeInt.negInt(exp) {
    | Some(exp) => ofRational(d, n, Pi(exp))
    | _ => ofDecimal(ratDecimal(n, d, c)->Decimal.inv)
    }
  | Rational(n, d, Exp(exp) as c) =>
    switch SafeInt.negInt(exp) {
    | Some(exp) => ofRational(d, n, Exp(exp))
    | _ => ofDecimal(ratDecimal(n, d, c)->Decimal.inv)
    }
  | Rational(n, d, Sqrt(sqrt) as c) =>
    switch SafeInt.mulInt(n, sqrt) {
    | Some(n) => ofRational(d, n, c)
    | None => ofDecimal(ratDecimal(n, d, c)->Decimal.inv)
    }
  | _ => ofDecimal(toDecimal(a)->Decimal.inv)
  }

let neg = a =>
  switch a {
  | Rational(n, d, c) =>
    switch SafeInt.negInt(n) {
    | Some(n) => ofRational(n, d, c)
    | _ => ofDecimal(ratDecimal(n, d, c)->Decimal.neg)
    }
  | Decimal(f) => ofDecimal(Decimal.neg(f))
  }

let abs = a =>
  switch a {
  | Rational(n, d, c) =>
    switch SafeInt.absInt(n) {
    | Some(n) => ofRational(n, d, c)
    | _ => ofDecimal(Decimal.abs(ratDecimal(n, d, c)))
    }
  | Decimal(f) => ofDecimal(Decimal.abs(f))
  }

let add = (a, b) => {
  let rat = switch (a, b) {
  | (Rational(an, ad, c), Rational(bn, bd, bc)) if Real_Constant.eq(c, bc) =>
    open SafeInt
    let ad = ofInt(ad)
    let bd = ofInt(bd)
    let n = ofInt(an) * bd + ofInt(bn) * ad
    let d = ad * bd
    safeRat(n, d, c)
  | _ => None
  }
  switch rat {
  | Some(rat) => rat
  | None => ofDecimal(Decimal.add(toDecimal(a), toDecimal(b)))
  }
}

let sub = (a, b) => {
  let rat = switch (a, b) {
  | (Rational(an, ad, c), Rational(bn, bd, bc)) if Real_Constant.eq(c, bc) =>
    open SafeInt
    let ad = ofInt(ad)
    let bd = ofInt(bd)
    let n = ofInt(an) * bd - ofInt(bn) * ad
    let d = ad * bd
    safeRat(n, d, c)
  | _ => None
  }
  switch rat {
  | Some(rat) => rat
  | None => ofDecimal(Decimal.sub(toDecimal(a), toDecimal(b)))
  }
}

%%private(
  let mulRat = (an, ad, bn, bd, c) => {
    open SafeInt
    let n = ofInt(an) * ofInt(bn)
    let d = ofInt(ad) * ofInt(bd)
    safeRat(n, d, c)
  }
)

let mul = (a, b) => {
  let rat = switch (a, b) {
  | (Rational(an, ad, c), Rational(bn, bd, Unit))
  | (Rational(an, ad, Unit), Rational(bn, bd, c)) =>
    mulRat(an, ad, bn, bd, c)
  | (Rational(an, ad, Pi(aExp)), Rational(bn, bd, Pi(bExp))) =>
    switch SafeInt.addInt(aExp, bExp) {
    | Some(exp) => mulRat(an, ad, bn, bd, Pi(exp))
    | _ => None
    }
  | (Rational(an, ad, Exp(aExp)), Rational(bn, bd, Exp(bExp))) =>
    switch SafeInt.addInt(aExp, bExp) {
    | Some(exp) => mulRat(an, ad, bn, bd, Exp(exp))
    | _ => None
    }
  | (Rational(an, ad, Sqrt(aSqrt)), Rational(bn, bd, Sqrt(bSqrt))) =>
    switch SafeInt.mulInt(aSqrt, bSqrt) {
    | Some(sqrt) => mulRat(an, ad, bn, bd, Sqrt(sqrt))
    | _ => None
    }
  | _ => None
  }
  switch rat {
  | Some(rat) => rat
  | None => ofDecimal(Decimal.mul(toDecimal(a), toDecimal(b)))
  }
}

let div = (a, b) => mul(a, inv(b))

%%private(
  let powRat = (~absB, n, d, c) => {
    open SafeInt
    let n = (ofInt(n) ** ofInt(absB))->toInt
    let d = (ofInt(d) ** ofInt(absB))->toInt
    switch (n, d) {
    | (Some(n), Some(d)) => Some(ofRational(n, d, c))
    | _ => None
    }
  }
)

let powInt = (a, b) => {
  let powAbsB = switch (a, IntUtil.abs(b)) {
  | (Rational(0, _, Unit), 0) => Some(nan)
  | (Rational(_), 0) => Some(ofInt(1))
  | (_, 1) => Some(a)
  | (_, 2) => Some(mul(a, a))
  | (Rational(n, d, Unit), absB) => powRat(~absB, n, d, Unit)
  | (Rational(n, d, Pi(aExp)), absB) =>
    switch SafeInt.mulInt(aExp, absB) {
    | Some(exp) => powRat(~absB, n, d, Pi(exp))
    | _ => None
    }
  | (Rational(n, d, Exp(aExp)), absB) =>
    switch SafeInt.mulInt(aExp, absB) {
    | Some(exp) => powRat(~absB, n, d, Exp(exp))
    | _ => None
    }
  | (Rational(n, d, Sqrt(aSqrt)), absB) =>
    switch SafeInt.powInt(aSqrt, absB) {
    | Some(aSqrt) => powRat(~absB, n, d, Sqrt(aSqrt))
    | _ => None
    }
  | _ => None
  }

  switch powAbsB {
  | Some(value) => b >= 0 ? value : inv(value)
  | None =>
    open Decimal
    ofDecimal(toDecimal(a) ** ofInt(b))
  }
}
