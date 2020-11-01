open Real_Types;
open Real_Base;

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
