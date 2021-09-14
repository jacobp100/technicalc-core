open Value_Types

external ofPercentUnsafe: percent => t = "%identity"
external ofScalarUnsafe: Scalar.t => t = "%identity"
external ofMatrixUnsafe: matrix => t = "%identity"
external ofVectorUnsafe: vector => t = "%identity"

let zero: t = #Z
let one: t = ofScalarUnsafe(Scalar.one)
let minusOne: t = ofScalarUnsafe(Scalar.minusOne)
let i: t = ofScalarUnsafe(Scalar.i)
let minusI: t = ofScalarUnsafe(Scalar.minusI)
let pi: t = ofScalarUnsafe(Scalar.pi)
let e: t = ofScalarUnsafe(Scalar.e)
let nan: t = ofScalarUnsafe(Scalar.nan)

let isNaN = (x: t) => x == #N

let eq = (a: t, b: t): bool =>
  switch (a, b) {
  | (#...Scalar.t as aS, #...Scalar.t as bS) => Scalar.eq(aS, bS)
  | (#P(aP), #P(bP)) => Scalar.eq(Scalar.Finite.toScalar(aP), Scalar.Finite.toScalar(bP))
  | (#V(aV), #V(bV)) => Vector.eq(aV, bV)
  | (#M(aM), #M(bM)) => Matrix.eq(aM, bM)
  | _ => false
  }

let normalize = (a: t): t =>
  switch a {
  | #...Scalar.t as s => s
  | #P(_) as p => p
  | #V(vector) as v => Vector.isEmpty(vector) ? #N : v
  | #M(matrix) as m => Matrix.isEmpty(matrix) ? #N : m
  }

// Note - as an optimization, `normalize` is only called when strictly needed
let ofScalar = (a: Scalar.t): t => ofScalarUnsafe(a)
let ofReal = (a: Real.t): t => ofScalarUnsafe(Scalar.ofReal(a))
let ofImag = (a: Real.t): t => ofScalarUnsafe(Scalar.ofImag(a))
let ofComplex = (re: Real.t, im: Real.t): t => ofScalarUnsafe(Scalar.ofComplex(re, im))
let ofPercent = (a: Scalar.t): t =>
  switch Scalar.Finite.ofScalar(a) {
  | Some(a) => #P(a)
  | None => nan
  }
let ofVector = (a: Vector.t): t => normalize(#V(a))
let ofMatrix = (a: Matrix.t): t => normalize(#M(a))

let ofDecimal = (a): t => ofScalarUnsafe(Scalar.ofDecimal(a))
let ofInt = (a): t => ofScalarUnsafe(Scalar.ofInt(a))
let ofFloat = (a): t => ofScalarUnsafe(Scalar.ofFloat(a))

let toDecimal = (a: t): Decimal.t =>
  switch a {
  | #...Scalar.t as s => Scalar.toDecimal(s)
  | _ => Decimal.nan
  }

let toInt = (a: t): option<int> =>
  switch a {
  | #...Scalar.t as s => Scalar.toInt(s)
  | _ => None
  }

let toFloat = (a: t): float =>
  switch a {
  | #...Scalar.t as s => Scalar.toFloat(s)
  | _ => Pervasives.nan
  }
