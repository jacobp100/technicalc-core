type t =
  | Unit
  | Pi(int)
  | Exp(int)
  | Sqrt(int);

let toDecimal = a =>
  switch (a) {
  | Unit => Decimal.ofInt(1)
  | Pi(ac) => Decimal.(pow(pi, ofInt(ac)))
  | Exp(ac) => Decimal.(ofInt(ac)->exp)
  | Sqrt(ac) => Decimal.(ofInt(ac)->sqrt)
  };

type simplificationState =
  | None
  | Zero
  | Factor(int, t);

let%private simplifySqrt = ac =>
  switch (ac) {
  | 0 => Zero
  | 1 => Factor(1, Unit)
  | _ =>
    let sqrtArg = ref(ac);
    let multiplier = ref(1);

    let limit = Belt.Float.fromInt(ac)->sqrt->ceil->FloatUtil.asIntExn;
    for (currentSqrtValue in 2 to limit) {
      let factor = currentSqrtValue * currentSqrtValue;

      while (sqrtArg^ mod factor == 0) {
        sqrtArg := sqrtArg^ / factor;
        multiplier := multiplier^ * currentSqrtValue;
      };
    };

    let constant = sqrtArg^ == 1 ? Unit : Sqrt(sqrtArg^);
    multiplier^ != 1 ? Factor(multiplier^, constant) : None;
  };

let%private simplifyConstantExponent = a =>
  switch (a) {
  | 0 => Factor(1, Unit)
  | _ => None
  };

let simplify = a =>
  switch (a) {
  | Sqrt(ac) => simplifySqrt(ac)
  | Pi(ac)
  | Exp(ac) => simplifyConstantExponent(ac)
  | _ => None
  };

let equal = (a, b) =>
  switch (a, b) {
  | (Unit, Unit) => true
  | (Pi(a), Pi(b))
  | (Exp(a), Exp(b))
  | (Sqrt(a), Sqrt(b)) => a == b
  | _ => false
  };

let (==) = equal;
