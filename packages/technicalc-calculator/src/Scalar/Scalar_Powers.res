open Scalar_Types
open Scalar_Base
open Scalar_Arithmetic
open Scalar_Functions
open Scalar_Exponentiation

let pow = (a: t, b: t): t =>
  switch (a, b) {
  | (#Zero, #Zero) => nan
  | (#Zero, #Real(_) | #Imag(_) | #Cmpx(_)) => zero
  | (#Real(_) | #Imag(_) | #Cmpx(_), #Zero) => one
  | (#Real(Rational(1, 1, Exp(1))), _) => exp(b)
  | (_, #Real(Rational(1, 2, Unit))) => sqrt(a)
  | (_, #Real(Rational(2, 1, Unit))) => mul(a, a)
  | (#Real(re), #Real(Rational(bInt, 1, Unit))) => ofReal(Real.powInt(re, bInt))
  | (#Imag(im), #Real(Rational(bInt, 1, Unit))) =>
    let aPowB = Real.powInt(im, bInt)
    switch IntUtil.safeMod(bInt, 4) {
    | 0 => ofReal(aPowB)
    | 1 => ofImag(aPowB)
    | 2 => ofReal(Real.neg(aPowB))
    | 3 => ofImag(Real.neg(aPowB))
    | _ => raise(Not_found)
    }
  | (#Real(_) | #Imag(_) | #Cmpx(_), #Real(_) | #Imag(_) | #Cmpx(_)) => mul(log(a), b)->exp
  | (#NaNN, _) | (_, #NaNN) => nan
  }
