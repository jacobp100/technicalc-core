open Value_Types

let zero: t = (Scalar.zero :> t)
let one: t = (Scalar.one :> t)
let minusOne: t = (Scalar.minusOne :> t)
let i: t = (Scalar.i :> t)
let minusI: t = (Scalar.minusI :> t)
let pi: t = (Scalar.pi :> t)
let e: t = (Scalar.e :> t)
let nan: t = (Scalar.nan :> t)

let isNaN = (x: t) => x == #NaNN

let eq = (a: t, b: t): bool =>
  switch (a, b) {
  | (#...Scalar.t as aS, #...Scalar.t as bS) => Scalar.eq(aS, bS)
  | (#Pcnt(aP), #Pcnt(bP)) => Scalar.eq(Scalar.Finite.toScalar(aP), Scalar.Finite.toScalar(bP))
  | (#Vect(aV), #Vect(bV)) => Vector.eq(aV, bV)
  | (#Matx(aM), #Matx(bM)) => Matrix.eq(aM, bM)
  | _ => false
  }

let ofScalar = (a: Scalar.t): t => (a :> t)
let ofPercent = (a: Scalar.t): t =>
  switch Scalar.Finite.ofScalar(a) {
  | Some(a) => #Pcnt(a)
  | None => nan
  }
let ofVector = (a: Vector.t): t => !Vector.isEmpty(a) ? #Vect(a) : nan
let ofMatrix = (a: Matrix.t): t => !Matrix.isEmpty(a) ? #Matx(a) : nan

let ofReal = (a: Real.t): t => (Scalar.ofReal(a) :> t)
let ofImag = (a: Real.t): t => (Scalar.ofImag(a) :> t)
let ofComplex = (re: Real.t, im: Real.t): t => (Scalar.ofComplex(re, im) :> t)

let ofDecimal = (a): t => (Scalar.ofDecimal(a) :> t)
let ofInt = (a): t => (Scalar.ofInt(a) :> t)
let ofFloat = (a): t => (Scalar.ofFloat(a) :> t)

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
