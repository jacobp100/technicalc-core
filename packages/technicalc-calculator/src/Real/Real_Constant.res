type t =
  | Unit
  | Pi(int)
  | Exp(int)
  | Sqrt(int)

let toDecimal = a =>
  switch a {
  | Unit => Decimal.ofInt(1)
  | Pi(ac) =>
    open Decimal
    pow(pi, ofInt(ac))
  | Exp(ac) =>
    open Decimal
    ofInt(ac)->exp
  | Sqrt(ac) =>
    open Decimal
    ofInt(ac)->sqrt
  }

type simplificationState =
  | None
  | Zero
  | Factor(int, t)

%%private(
  let simplifySqrt = ac =>
    switch ac {
    | 0 => Zero
    | 1 => Factor(1, Unit)
    | _ =>
      let sqrtArg = ref(ac)
      let multiplier = ref(1)

      let limit = Belt.Float.fromInt(ac)->sqrt->ceil->FloatUtil.toIntExn
      for currentSqrtValue in 2 to limit {
        let factor = currentSqrtValue * currentSqrtValue

        while mod(sqrtArg.contents, factor) == 0 {
          sqrtArg := sqrtArg.contents / factor
          multiplier := multiplier.contents * currentSqrtValue
        }
      }

      let constant = sqrtArg.contents == 1 ? Unit : Sqrt(sqrtArg.contents)
      multiplier.contents != 1 ? Factor(multiplier.contents, constant) : None
    }
)

%%private(
  let simplifyConstantExponent = a =>
    switch a {
    | 0 => Factor(1, Unit)
    | _ => None
    }
)

let simplify = a =>
  switch a {
  | Sqrt(ac) => simplifySqrt(ac)
  | Pi(ac)
  | Exp(ac) =>
    simplifyConstantExponent(ac)
  | _ => None
  }

let eq = (a, b) =>
  switch (a, b) {
  | (Unit, Unit) => true
  | (Pi(a), Pi(b))
  | (Exp(a), Exp(b))
  | (Sqrt(a), Sqrt(b)) =>
    a == b
  | _ => false
  }

let \"=" = eq
