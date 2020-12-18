open Value_Types;
open Value_Base;

let (==) = Value_Base.equal;
let (~-) = Value_Arithmetic.neg;
let (+) = Value_Arithmetic.add;
let (-) = Value_Arithmetic.sub;
let ( * ) = Value_Arithmetic.mul;
let (/) = Value_Arithmetic.div;
let (exp, log) = Value_Exponentiation.(exp, log);
let percentToNumerical = Value_Util.percentToNumerical;

let halfS = `R(Real.Rational(1, 2, Unit));

let%private isSquare = x => float_of_int(x)->sqrt->FloatUtil.isInt;

let rec pow = (a: t, b: t): t =>
  switch (a, b) {
  | (`Z, `R(_) | `I(_) | `C(_)) => `Z
  | (`R(_) | `I(_) | `C(_), `Z) => one
  | (`Z, `Z) => `N
  | (`R(Rational(n, d, Unit)), `R(Rational(1, 2, Unit))) when isSquare(d) =>
    let denSqrt = float_of_int(d)->sqrt->FloatUtil.intValueExn;
    let r = Real.ofRational(1, denSqrt, Sqrt(IntUtil.abs(n)));
    if (n >= 0) {
      ofReal(r);
    } else {
      ofImag(r);
    };
  | (`R(Rational(1, 1, Exp(1))), _) => exp(b)
  | (`V(v), `R(Rational(2, 1, Unit))) =>
    Vector.magnitudeSquared(v)->ofScalar
  // | (_, `R(Rational(2, 1, Unit))) => a * a
  | (`R(re), `R(Rational(bInt, 1, Unit))) => ofReal(Real.powInt(re, bInt))
  | (`I(im), `R(Rational(bInt, 1, Unit))) =>
    let aPowB = Real.powInt(im, bInt);
    switch (IntUtil.safeMod(bInt, 4)) {
    | 0 => ofReal(aPowB)
    | 1 => ofImag(aPowB)
    | 2 => ofReal(Real.(- aPowB))
    | 3 => ofImag(Real.(- aPowB))
    | _ => raise(Not_found)
    };
  | (`R(_) | `I(_) | `C(_), `R(_) | `I(_) | `C(_)) => exp(log(a) * b)
  | (`P(aP), `P(bP)) => pow(percentToNumerical(aP), percentToNumerical(bP))
  | (`P(p), _) => pow(percentToNumerical(p), b)
  | (_, `P(p)) => pow(a, percentToNumerical(p))
  | (`M(m), `R(Rational((-1), 1, Unit))) => Matrix.inverse(m)->ofMatrix
  | (`M(m), `Z) when Pervasives.(m.numColumns == m.numRows) =>
    Matrix.identity(m.numRows)->ofMatrix
  | (`M(m), `R(Rational(gtZero, 1, Unit))) when gtZero >= 0 =>
    Matrix.pow(m, gtZero)->ofMatrix
  | (`N | `V(_) | `M(_), _)
  | (_, `N | `V(_) | `M(_)) => `N
  };

let sqrt = (x: t): t => pow(x, halfS);
