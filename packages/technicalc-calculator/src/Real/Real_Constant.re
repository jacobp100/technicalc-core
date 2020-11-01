type t =
  | Unit
  | Pi
  | Exp(int)
  | Sqrt(int);

let toDecimal = a =>
  switch (a) {
  | Unit => Decimal.ofInt(1)
  | Pi => Decimal.pi
  | Exp(ac) => Decimal.(ofInt(ac)->exp)
  | Sqrt(ac) => Decimal.(ofInt(ac)->sqrt)
  };

let simplifySqrt = ac =>
  switch (ac) {
  | 0 => `Z
  | 1 => `Factor((1, Unit))
  | _ =>
    let sqrtArg = ref(ac);
    let multiplier = ref(1);

    let limit = float_of_int(ac)->sqrt->ceil->FloatUtil.intValueExn;
    for (currentSqrtValue in 2 to limit) {
      let factor = currentSqrtValue * currentSqrtValue;

      while (sqrtArg^ mod factor == 0) {
        sqrtArg := sqrtArg^ / factor;
        multiplier := multiplier^ * currentSqrtValue;
      };
    };

    let constant = sqrtArg^ == 1 ? Unit : Sqrt(sqrtArg^);
    multiplier^ != 1 ? `Factor((multiplier^, constant)) : `None;
  };

let simplifyExp = a =>
  switch (a) {
  | 0 => `Factor((1, Unit))
  | _ => `None
  };

let simplify = a =>
  switch (a) {
  | Sqrt(ac) => simplifySqrt(ac)
  | Exp(0) => `Factor((1, Unit))
  | _ => `None
  };

let equal = (a, b) =>
  switch (a, b) {
  | (Unit, Unit)
  | (Pi, Pi) => true
  | (Sqrt(a), Sqrt(b))
  | (Exp(a), Exp(b)) => a == b
  | _ => false
  };

let (==) = equal;
