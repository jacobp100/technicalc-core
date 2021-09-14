open Real_Types
open Real_Util

let one = Rational(1, 1, Unit)
let minusOne = Rational(-1, 1, Unit)
let zero = Rational(0, 1, Unit)
let nan = Rational(1, 0, Unit)
let pi = Rational(1, 1, Pi(1))
let e = Rational(1, 1, Exp(1))

let isNaN = a =>
  switch a {
  | Rational(_, 0, _) => true
  | Decimal(f) => !Decimal.isFinite(f)
  | _ => false
  }

let eq = (a, b) =>
  switch (a, b) {
  | (Rational(an, ad, ac), Rational(bn, bd, bc)) => an == bn && ad == bd && Real_Constant.eq(ac, bc)
  | (Decimal(af), Decimal(bf)) => Decimal.eq(af, bf)
  | _ => false
  }

let normalize = a =>
  switch a {
  | Rational(_, 0, _) => nan
  | Decimal(f) if Decimal.eq(f, Decimal.zero) => zero
  | Decimal(f) if !Decimal.isFinite(f) => nan
  | _ => a
  }

let ofDecimal = f => Decimal(f)->normalize

let ofInt = n => {
  assert (n->Belt.Float.fromInt->Belt.Int.fromFloat == n)
  Rational(n, 1, Unit)
}

let ofRational = (n, d, c) =>
  if d == 0 {
    nan
  } else {
    let n = d >= 0 ? n : -n
    let d = IntUtil.abs(d)
    let gcd = gcd(IntUtil.abs(n), d)
    let n = n / gcd
    let d = d / gcd

    switch Real_Constant.simplify(c) {
    | None => Rational(n, d, c)
    | Zero => Rational(0, 1, Unit)
    | Factor(n', c) =>
      switch SafeInt.mulInt(n, n') {
      | Some(n) => Rational(n, d, c)
      | None => Decimal(ratDecimal(d, d, c))
      }
    }
  }

let toDecimal = a =>
  switch a {
  | Rational(n, d, c) => ratDecimal(n, d, c)
  | Decimal(d) => d
  }

let toInt = a =>
  switch a {
  | Rational(n, 1, Unit) => Some(n)
  | Rational(_) => None
  | Decimal(d) => Decimal.toFloat(d)->FloatUtil.toInt
  }

let gt = (a, b) => Decimal.gt(toDecimal(a), toDecimal(b))
let gte = (a, b) => Decimal.gte(toDecimal(a), toDecimal(b))
let lt = (a, b) => Decimal.lt(toDecimal(a), toDecimal(b))
let lte = (a, b) => Decimal.lte(toDecimal(a), toDecimal(b))
