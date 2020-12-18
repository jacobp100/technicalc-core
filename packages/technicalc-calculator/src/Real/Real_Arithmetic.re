open Real_Types;
open Real_Base;
open Real_Util;

/* Disable ops that can overflow */
// let (+) = None;
// let (-) = None;
// let ( * ) = None;
// let (~-) = None;

let%private safeRat = (n, d, c) => {
  switch (SafeInt.toInt(n), SafeInt.toInt(d)) {
  | (Some(n), Some(d)) => Some(ofRational(n, d, c))
  | _ => None
  };
};

let%private inv = a => {
  switch (a) {
  | Rational(n, d, Unit) => ofRational(d, n, Unit)
  | Rational(n, d, Exp(exp) as c) =>
    switch (SafeInt.negInt(exp)) {
    | Some(exp) => ofRational(d, n, Exp(exp))
    | _ => ofDecimal(ratDecimal(n, d, c)->Decimal.inv)
    }
  | _ => ofDecimal(toDecimal(a)->Decimal.inv)
  };
};

let neg = a =>
  switch (a) {
  | Rational(n, d, c) =>
    switch (SafeInt.negInt(n)) {
    | Some(n) => ofRational(n, d, c)
    | _ => ofDecimal(ratDecimal(n, d, c)->Decimal.neg)
    }
  | Decimal(f) => ofDecimal(Decimal.neg(f))
  };

let abs = a =>
  switch (a) {
  | Rational(n, d, c) =>
    switch (SafeInt.absInt(n)) {
    | Some(n) => ofRational(n, d, c)
    | _ => ofDecimal(Decimal.abs(ratDecimal(n, d, c)))
    }
  | Decimal(f) => ofDecimal(Decimal.abs(f))
  };

let add = (a, b) => {
  let rat =
    switch (a, b) {
    | (Rational(an, ad, c), Rational(bn, bd, bc))
        when Real_Constant.(c == bc) =>
      open SafeInt;
      let ad = ofInt(ad);
      let bd = ofInt(bd);
      let n = ofInt(an) * bd + ofInt(bn) * ad;
      let d = ad * bd;
      safeRat(n, d, c);
    | _ => None
    };
  switch (rat) {
  | Some(rat) => rat
  | None => ofDecimal(Decimal.add(toDecimal(a), toDecimal(b)))
  };
};

let sub = (a, b) => {
  let rat =
    switch (a, b) {
    | (Rational(an, ad, c), Rational(bn, bd, bc))
        when Real_Constant.(c == bc) =>
      open SafeInt;
      let ad = ofInt(ad);
      let bd = ofInt(bd);
      let n = ofInt(an) * bd - ofInt(bn) * ad;
      let d = ad * bd;
      safeRat(n, d, c);
    | _ => None
    };
  switch (rat) {
  | Some(rat) => rat
  | None => ofDecimal(Decimal.sub(toDecimal(a), toDecimal(b)))
  };
};

let%private mulRat = (an, ad, bn, bd, c) => {
  open SafeInt;
  let n = ofInt(an) * ofInt(bn);
  let d = ofInt(ad) * ofInt(bd);
  safeRat(n, d, c);
};

let mul = (a, b) => {
  let rat =
    switch (a, b) {
    | (Rational(an, ad, c), Rational(bn, bd, Unit))
    | (Rational(an, ad, Unit), Rational(bn, bd, c)) =>
      mulRat(an, ad, bn, bd, c)
    | (Rational(an, ad, Exp(aExp)), Rational(bn, bd, Exp(bExp))) =>
      switch (SafeInt.addInt(aExp, bExp)) {
      | Some(exp) => mulRat(an, ad, bn, bd, Exp(exp))
      | _ => None
      }
    | (Rational(an, ad, Sqrt(aSqrt)), Rational(bn, bd, Sqrt(bSqrt))) =>
      switch (SafeInt.mulInt(aSqrt, bSqrt)) {
      | Some(sqrt) => mulRat(an, ad, bn, bd, Sqrt(sqrt))
      | _ => None
      }
    | _ => None
    };
  switch (rat) {
  | Some(rat) => rat
  | None => ofDecimal(Decimal.mul(toDecimal(a), toDecimal(b)))
  };
};

let%private divRat = (an, ad, bn, bd, c) => {
  open SafeInt;
  let n = ofInt(an) * ofInt(bd);
  let d = ofInt(ad) * ofInt(bn);
  safeRat(n, d, c);
};

let div = (a, b) => {
  let rat =
    switch (a, b) {
    | (_, Rational(0, _, _)) => Some(nan)
    | (Rational(an, ad, ac), Rational(bn, bd, bc))
        when Real_Constant.(ac == bc) =>
      divRat(an, ad, bn, bd, Unit)
    | (Rational(an, ad, c), Rational(bn, bd, Unit)) =>
      divRat(an, ad, bn, bd, c)
    | (Rational(an, ad, Exp(aExp)), Rational(bn, bd, Exp(bExp))) =>
      switch (SafeInt.subInt(aExp, bExp)) {
      | Some(exp) => divRat(an, ad, bn, bd, Exp(exp))
      | _ => None
      }
    | (Rational(an, ad, Sqrt(aSqrt)), Rational(bn, bd, Sqrt(bSqrt)))
        when aSqrt > bSqrt && aSqrt mod bSqrt == 0 =>
      switch (SafeInt.divInt(aSqrt, bSqrt)) {
      | Some(sqrt) => divRat(an, ad, bn, bd, Sqrt(sqrt))
      | _ => None
      }
    | _ => None
    };
  switch (rat) {
  | Some(rat) => rat
  | None => ofDecimal(Decimal.div(toDecimal(a), toDecimal(b)))
  };
};

let powInt = (a, b) => {
  let powAbsB =
    switch (a, IntUtil.abs(b)) {
    | (Rational(0, 0, Unit), 0) => Some(nan)
    | (Rational(_), 0) => Some(ofInt(1))
    | (_, 1) => Some(a)
    | (_, 2) => Some(mul(a, a))
    | (Rational(n, d, Unit), b) =>
      open SafeInt;
      let n = (ofInt(n) ** ofInt(b))->toInt;
      let d = (ofInt(d) ** ofInt(b))->toInt;
      switch (n, d) {
      | (Some(n), Some(d)) => Some(ofRational(n, d, Unit))
      | _ => None
      };
    | _ => None
    };
  switch (powAbsB) {
  | Some(value) => b >= 0 ? value : inv(value)
  | None => ofDecimal(Decimal.(toDecimal(a) ** ofInt(b)))
  };
};
