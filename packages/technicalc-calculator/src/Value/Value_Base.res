open Value_Types

let zero: t = (Scalar.zero :> t)
let one: t = (Scalar.one :> t)
let minusOne: t = (Scalar.minusOne :> t)
let i: t = (Scalar.i :> t)
let minusI: t = (Scalar.minusI :> t)
let pi: t = (Scalar.pi :> t)
let e: t = (Scalar.e :> t)
let nan: t = (Scalar.nan :> t)

let xUnit: t = #Vect(Vector.xUnit)
let yUnit: t = #Vect(Vector.yUnit)
let zUnit: t = #Vect(Vector.zUnit)

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

let ofReal = (a: Real.t): t => (Scalar.ofReal(a) :> t)
let ofImag = (a: Real.t): t => (Scalar.ofImag(a) :> t)
let ofComplex = (re: Real.t, im: Real.t): t => (Scalar.ofComplex(re, im) :> t)

let ofDecimal = (a): t => (Scalar.ofDecimal(a) :> t)
let ofInt = (a): t => (Scalar.ofInt(a) :> t)
let ofFloat = (a): t => (Scalar.ofFloat(a) :> t)

let ofVector = (a: Vector.t): t =>
  switch a {
  | [] => nan
  | [element] => ofScalar((element :> Scalar.t))
  | _ => #Vect(a)
  }
let ofMatrix = (a: Matrix.t): t =>
  switch a.elements {
  | [] => nan
  | [element] => ofScalar((element :> Scalar.t))
  | elements if a.numColumns == 1 => ofVector(elements)
  | _ => #Matx(a)
  }
let ofMeasure = (a: Measure.t): t =>
  if Belt.Array.length(a.units) == 0 {
    ofReal(a.value)
  } else {
    #Mesr(a)
  }

let toReal = (a: t): Real.t =>
  switch a {
  | #...Scalar.t as s => Scalar.toReal(s)
  | _ => Real.nan
  }

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
