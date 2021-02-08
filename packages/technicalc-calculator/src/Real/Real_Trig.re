open Real_Types;
open Real_Base;
open Real_Util;

let mod2Pi = a =>
  switch (a) {
  | Rational(n, d, Pi(1)) =>
    let n = IntUtil.safeMod(n, 2 * d);
    Rational(n, d, Pi(1));
  | _ => a
  };

let sin = a =>
  switch (mod2Pi(a)) {
  | Rational(0, 1, _)
  | Rational(1 | 2, 1, Pi(1)) => zero
  | Rational(1, 2, Pi(1)) => one
  | Rational(3, 2, Pi(1)) => minusOne
  | Rational(1 | 2, 3, Pi(1)) => ofRational(1, 2, Sqrt(3))
  | Rational(4 | 5, 3, Pi(1)) => ofRational(-1, 2, Sqrt(3))
  | Rational(1 | 3, 4, Pi(1)) => ofRational(1, 2, Sqrt(2))
  | Rational(5 | 7, 4, Pi(1)) => ofRational(-1, 2, Sqrt(2))
  | Rational(1 | 5, 6, Pi(1)) => ofRational(1, 2, Unit)
  | Rational(7 | 11, 6, Pi(1)) => ofRational(-1, 2, Unit)
  | Rational(n, d, c) => Decimal(ratDecimal(n, d, c)->Decimal.sin)
  | Decimal(f) => Decimal(Decimal.sin(f))
  };

let cos = a =>
  switch (mod2Pi(a)) {
  | Rational(0, 1, _)
  | Rational(2, 1, Pi(1)) => one
  | Rational(1, 1, Pi(1)) => minusOne
  | Rational(1 | 3, 2, Pi(1)) => zero
  | Rational(1 | 5, 3, Pi(1)) => ofRational(1, 2, Unit)
  | Rational(2 | 4, 3, Pi(1)) => ofRational(-1, 2, Unit)
  | Rational(1 | 7, 4, Pi(1)) => ofRational(1, 2, Sqrt(2))
  | Rational(3 | 5, 4, Pi(1)) => ofRational(-1, 2, Sqrt(2))
  | Rational(1 | 11, 6, Pi(1)) => ofRational(1, 2, Sqrt(3))
  | Rational(5 | 7, 6, Pi(1)) => ofRational(-1, 2, Sqrt(3))
  | Rational(n, d, c) => Decimal(ratDecimal(n, d, c)->Decimal.cos)
  | Decimal(f) => Decimal(Decimal.cos(f))
  };
