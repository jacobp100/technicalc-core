open Scalar_Types
open Scalar_Base
open Scalar_Arithmetic
open Scalar_Functions
open Scalar_Exponentiation

let pow = (a: t, b: t): t =>
  switch (a, b) {
  | (#Z, #Z) => nan
  | (#Z, #R(_) | #I(_) | #C(_)) => zero
  | (#R(_) | #I(_) | #C(_), #Z) => one
  | (#R(Rational(1, 1, Exp(1))), _) => exp(b)
  | (_, #R(Rational(1, 2, Unit))) => sqrt(a)
  | (_, #R(Rational(2, 1, Unit))) => mul(a, a)
  | (#R(re), #R(Rational(bInt, 1, Unit))) => ofReal(Real.powInt(re, bInt))
  | (#I(im), #R(Rational(bInt, 1, Unit))) =>
    let aPowB = Real.powInt(im, bInt)
    switch IntUtil.safeMod(bInt, 4) {
    | 0 => ofReal(aPowB)
    | 1 => ofImag(aPowB)
    | 2 => ofReal(Real.neg(aPowB))
    | 3 => ofImag(Real.neg(aPowB))
    | _ => raise(Not_found)
    }
  | (#R(_) | #I(_) | #C(_), #R(_) | #I(_) | #C(_)) => mul(log(a), b)->exp
  | (#N, _) | (_, #N) => nan
  }
