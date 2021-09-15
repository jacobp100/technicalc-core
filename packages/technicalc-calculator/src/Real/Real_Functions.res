open Real_Types
open Real_Base

let sqrt = a => {
  let value = switch a {
  | Rational(n, d, Unit) =>
    switch SafeInt.mulInt(n, d) {
    | Some(nd) => ofRational(1, d, Sqrt(nd))->Some
    | None => None
    }
  | Rational(_)
  | Decimal(_) =>
    None
  }

  switch value {
  | Some(value) => value
  | None => toDecimal(a)->Decimal.sqrt->ofDecimal
  }
}

%%private(
  let ofDecimalInt = f =>
    switch Decimal.toFloat(f)->FloatUtil.asInt {
    | Some(intVal) => ofRational(intVal, 1, Unit)
    | None => ofDecimal(f)
    }
)

let round = a => toDecimal(a)->Decimal.round->ofDecimalInt
let floor = a => toDecimal(a)->Decimal.floor->ofDecimalInt
let ceil = a => toDecimal(a)->Decimal.ceil->ofDecimalInt

%%private(let oneDeg = ofRational(1, 180, Pi(1)))
%%private(let oneArcMinute = ofRational(1, 10800, Pi(1)))
%%private(let oneArcSecond = ofRational(1, 648000, Pi(1)))
%%private(let oneGrad = ofRational(1, 200, Pi(1)))

let toDeg = a => Real_Arithmetic.div(a, oneDeg)
let toArcMinute = a => Real_Arithmetic.div(a, oneArcMinute)
let toArcSecond = a => Real_Arithmetic.div(a, oneArcSecond)
let toGrad = a => Real_Arithmetic.div(a, oneGrad)

let ofDeg = a => Real_Arithmetic.mul(a, oneDeg)
let ofArcMin = a => Real_Arithmetic.mul(a, oneArcMinute)
let ofArcSec = a => Real_Arithmetic.mul(a, oneArcSecond)
let ofGrad = a => Real_Arithmetic.mul(a, oneGrad)

let max = (a: t, b: t): t => gte(a, b) ? a : b
let min = (a: t, b: t): t => lte(a, b) ? a : b

let gcd = (a: t, b: t): t =>
  switch (toInt(a), toInt(b)) {
  | (Some(a), Some(b)) => ofInt(IntUtil.gcd(IntUtil.abs(a), IntUtil.abs(b)))
  | _ => nan
  }

let lcm = (a: t, b: t): t =>
  switch (toInt(a), toInt(b)) {
  | (Some(0), Some(_))
  | (Some(_), Some(0)) => zero
  | (Some(a), Some(b)) =>
    let gcd = IntUtil.gcd(IntUtil.abs(a), IntUtil.abs(b))
    switch {
      open SafeInt
      toInt(ofInt(a) * ofInt(b) / ofInt(gcd))
    } {
    | Some(ans) => ofInt(ans)
    | None => nan
    }
  | _ => nan
  }
