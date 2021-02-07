open Real_Types;
open Real_Base;

let%private ofDecimalInt = f =>
  switch (Decimal.toFloat(f)->FloatUtil.intValue) {
  | Some(intVal) => ofRational(intVal, 1, Unit)
  | None => ofDecimal(f)
  };

let round = a => toDecimal(a)->Decimal.round->ofDecimalInt;
let floor = a => toDecimal(a)->Decimal.floor->ofDecimalInt;
let ceil = a => toDecimal(a)->Decimal.ceil->ofDecimalInt;

let%private oneDeg = Rational(1, 180, Pi(1));
let%private oneArcMinute = Rational(1, 10800, Pi(1));
let%private oneArcSecond = Rational(1, 648000, Pi(1));
let%private oneGrad = Rational(1, 200, Pi(1));

let toDeg = a => Real_Arithmetic.div(a, oneDeg);
let toArcMinute = a => Real_Arithmetic.div(a, oneArcMinute);
let toArcSecond = a => Real_Arithmetic.div(a, oneArcSecond);
let toGrad = a => Real_Arithmetic.div(a, oneGrad);

let ofDeg = a => Real_Arithmetic.mul(a, oneDeg);
let ofArcMin = a => Real_Arithmetic.mul(a, oneArcMinute);
let ofArcSec = a => Real_Arithmetic.mul(a, oneArcSecond);
let ofGrad = a => Real_Arithmetic.mul(a, oneGrad);

let max = (a: t, b: t): t => gte(a, b) ? a : b;
let min = (a: t, b: t): t => lte(a, b) ? a : b;

let gcd = (a: t, b: t): t =>
  switch (toInt(a), toInt(b)) {
  | (Some(a), Some(b)) =>
    ofInt(Real_Util.gcd(IntUtil.abs(a), IntUtil.abs(b)))
  | _ => nan
  };

let lcm = (a: t, b: t): t =>
  switch (toInt(a), toInt(b)) {
  | (Some(0), Some(_))
  | (Some(_), Some(0)) => zero
  | (Some(a), Some(b)) =>
    let gcd = Real_Util.gcd(IntUtil.abs(a), IntUtil.abs(b));
    switch (SafeInt.(toInt(ofInt(a) * ofInt(b) / ofInt(gcd)))) {
    | Some(ans) => ofInt(ans)
    | None => nan
    };
  | _ => nan
  };
