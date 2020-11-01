open Scalar_Types;

let one: t = `R(Real.one);
let minusOne: t = `R(Real.minusOne);
let zero: t = `R(Real.zero);
let nan: t = `R(Real.nan);

let normalize = (v: t): t =>
  switch (v) {
  | `Z
  | `R(Rational(0, _, _))
  | `I(Rational(0, _, _))
  | `C(Rational(0, _, _), Rational(0, _, _)) => `Z
  | `C(Rational(0, _, _), v) => `I(v)
  | `C(v, Rational(0, _, _)) => `R(v)
  | `R(_)
  | `I(_)
  | `C(_) => v
  };

let isNaN = (a: t) =>
  switch (a) {
  | `Z => false
  | `R(v)
  | `I(v) => Real.isNaN(v)
  | `C(re, im) => Real.isNaN(re) || Real.isNaN(im)
  };

let ofFloat = (v): t =>
  switch (classify_float(v)) {
  | FP_normal
  | FP_subnormal =>
    let magnitude = 1.e6;
    let intMaxF = float_of_int(max_int);
    let numeratorF = v *. magnitude;
    switch (FloatUtil.intValue(numeratorF), FloatUtil.intValue(magnitude)) {
    | (Some(numerator), Some(denominator))
        when abs_float(numeratorF) < intMaxF =>
      `R(Real.ofRational(numerator, denominator, Unit))->normalize
    | _ => `R(Real.ofDecimal(Decimal.ofFloat(v)))->normalize
    };
  | FP_zero => `Z
  | FP_infinite
  | FP_nan => nan
  };

let toDecimal = (a: t): Decimal.t =>
  switch (a) {
  | `Z => Decimal.zero
  | `R(re) => Real.toDecimal(re)
  | _ => Decimal.nan
  };

let toInt = (a: t): option(int) =>
  switch (a) {
  | `Z => Some(0)
  | `R(re) => Real.toInt(re)
  | _ => None
  };

let toFloat = (a: t): float =>
  switch (a) {
  | `Z => 0.
  | `R(re) => Real.toDecimal(re)->Decimal.toFloat
  | _ => Pervasives.nan
  };

let equal = (a: t, b: t): bool =>
  switch (a, b) {
  | (`Z, `Z) => true
  | (`R(a), `R(b))
  | (`I(a), `I(b)) => Real.(a == b)
  | (`C(aRe, aIm), `C(bRe, bIm)) => Real.(aRe == bRe && aIm == bIm)
  | _ => false
  };

let map = (a: t, f: Real.t => Real.t): t =>
  switch (a) {
  | `Z => `Z
  | `R(re) => `R(f(re))
  | `I(im) => `I(f(im))
  | `C(re, im) => `C((f(re), f(im)))
  };
