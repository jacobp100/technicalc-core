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

let deg = a => Real_Arithmetic.div(a, Rational(1, 180, Pi));
let grad = a => Real_Arithmetic.div(a, Rational(1, 200, Pi));

let max = (a: t, b: t): t => gte(a, b) ? a : b;
let min = (a: t, b: t): t => lte(a, b) ? a : b;

let gcd = (a: t, b: t): t =>
  switch (toInt(a), toInt(b)) {
  | (Some(a), Some(b)) => ofInt(Real_Util.gcd(abs(a), abs(b)))
  | _ => nan
  };

let lcm = (a: t, b: t): t =>
  switch (toInt(a), toInt(b)) {
  | (Some(0), Some(_))
  | (Some(_), Some(0)) => zero
  | (Some(a), Some(b)) =>
    let gcd = Real_Util.gcd(abs(a), abs(b));
    switch (SafeInt.(toInt(ofInt(a) * ofInt(b) / ofInt(gcd)))) {
    | Some(ans) => ofInt(ans)
    | None => nan
    };
  | _ => nan
  };
