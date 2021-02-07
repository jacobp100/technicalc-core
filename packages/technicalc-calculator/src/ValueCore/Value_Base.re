open Value_Types;

external ofPercentUnsafe: percent => t = "%identity";
external ofScalarUnsafe: Scalar.t => t = "%identity";
external ofMatrixUnsafe: matrix => t = "%identity";
external ofVectorUnsafe: vector => t = "%identity";

let zero: t = `Z;
let one: t = `R(Rational(1, 1, Unit));
let minusOne: t = `R(Rational(-1, 1, Unit));
let i: t = `I(Rational(1, 1, Unit));
let minusI: t = `I(Rational(-1, 1, Unit));
let pi: t = `R(Rational(1, 1, Pi(1)));
let e: t = `R(Rational(1, 1, Exp(1)));
let nan: t = `N;

let isNaN = (x: t) => x == `N;

let equal = (a: t, b: t): bool =>
  switch (a, b) {
  | (#Scalar.t as aS, #Scalar.t as bS) => Scalar.equal(aS, bS)
  | (`P(aP), `P(bP)) => Scalar.equal(aP, bP)
  | (`V(aV), `V(bV)) => Vector.equal(aV, bV)
  | (`M(aM), `M(bM)) => Matrix.equal(aM, bM)
  | _ => false
  };

let normalize = (a: t): t =>
  switch (a) {
  | `Z => `Z
  | (`R(v) | `I(v)) as scalar =>
    Real.isNaN(v) ? `N : Scalar.normalize(scalar)->ofScalarUnsafe
  | `C(re, im) as scalar =>
    Real.isNaN(re) || Real.isNaN(im)
      ? `N : Scalar.normalize(scalar)->ofScalarUnsafe
  | `P(p) =>
    if (Scalar.isNaN(p)) {
      `N;
    } else {
      let normalized = Scalar.normalize(p);
      normalized === p ? a : `P(Scalar.normalize(p));
    }
  | `V(vector) =>
    Vector.isEmpty(vector) || Vector.some(vector, Scalar.isNaN)
      ? `N : `V(Vector.map(vector, Scalar.normalize))
  | `M(matrix) =>
    Matrix.isEmpty(matrix) || Matrix.some(matrix, Scalar.isNaN)
      ? `N : `M(Matrix.map(matrix, Scalar.normalize))
  | `N => `N
  };

let ofScalar = (a: Scalar.t): t => ofScalarUnsafe(a)->normalize;
let ofReal = (a: Real.t): t => normalize(`R(a));
let ofImag = (a: Real.t): t => normalize(`I(a));
let ofComplex = (re: Real.t, im: Real.t): t => normalize(`C((re, im)));
let ofPercent = (a: Scalar.t): t => normalize(`P(a));
let ofVector = (a: Vector.t): t => normalize(`V(a));
let ofMatrix = (a: Matrix.t): t => normalize(`M(a));

let ofDecimal = (a): t => `R(Decimal(a))->normalize;
let ofInt = (a): t => `R(Real.ofInt(a))->normalize;
let ofFloat = (a): t => {
  let scalar = Scalar.ofFloat(a);
  Scalar.isNaN(scalar) ? `N : ofScalarUnsafe(scalar);
};

let toDecimal = (a: t): Decimal.t =>
  switch (a) {
  | #Scalar.t as s => Scalar.toDecimal(s)
  | _ => Decimal.nan
  };

let toInt = (a: t): option(int) =>
  switch (a) {
  | #Scalar.t as s => Scalar.toInt(s)
  | _ => None
  };

let toFloat = (a: t): float =>
  switch (a) {
  | #Scalar.t as s => Scalar.toFloat(s)
  | _ => Pervasives.nan
  };
