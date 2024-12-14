open Value_Types
open Value_Base

%%private(
  let mapScalar = (a: t, fn: Scalar.t => Scalar.t): t =>
    switch a {
    | #...Scalar.t as t => fn(t)->ofScalar
    | _ => nan
    }
)

%%private(
  let mapScalarElements = (a: t, fn: Scalar.t => Scalar.t): t =>
    switch a {
    | #...Scalar.t as t => fn(t)->ofScalar
    | #Pcnt(p) =>
      let p = Scalar.Finite.toScalar(p)
      fn(p)->ofPercent
    | #Matx(m) => Matrix.map(m, fn)->ofMatrix
    | #Vect(v) => Vector.map(v, fn)->ofVector
    | #Mesr({value, units}) =>
      Measure.ofReal(Scalar.ofReal(value)->(fn(_))->Scalar.toReal, ~units)->ofMeasure
    }
)

let dot = (a: t, b: t): t =>
  switch (a, b) {
  | (#Vect(aV), #Vect(bV)) => Vector.dot(aV, bV)->ofScalar
  | _ => Value_Arithmetic.mul(a, b)
  }

let inv = a =>
  switch (a: t) {
  | #...Scalar.t as t => Scalar.inv(t)->ofScalar
  | #Matx(m) => Matrix.inv(m)->ofMatrix
  | _ => nan
  }

let abs = (a: t): t =>
  switch a {
  | #Zero => zero // Optimisation to avoid extra function calls
  | #...Scalar.t as t => Scalar.abs(t)->ofScalar
  | #Pcnt(p) =>
    let p = Scalar.Finite.toScalar(p)
    Scalar.abs(p)->ofPercent
  | #Mesr({value, units}) => Measure.ofReal(Real.abs(value), ~units)->ofMeasure
  | #Matx(m) => Matrix.determinant(m)->ofScalar
  | #Vect(v) => Vector.magnitudeSquared(v)->Scalar.sqrt->ofScalar
  }

let sqrt = a => mapScalar(a, a => Scalar.sqrt(a))
let log = a => mapScalar(a, a => Scalar.log(a))
let exp = a => mapScalar(a, a => Scalar.exp(a))

let round = a => mapScalarElements(a, a => Scalar.round(a))
let floor = a => mapScalarElements(a, a => Scalar.floor(a))
let ceil = a => mapScalarElements(a, a => Scalar.ceil(a))

let ofDeg = a => mapScalarElements(a, a => Scalar.ofDeg(a))
let ofArcMin = a => mapScalarElements(a, a => Scalar.ofArcMin(a))
let ofArcSec = a => mapScalarElements(a, a => Scalar.ofArcSec(a))
let ofGrad = a => mapScalarElements(a, a => Scalar.ofGrad(a))

let toDeg = a => mapScalarElements(a, a => Scalar.toDeg(a))
let toArcMinute = a => mapScalarElements(a, a => Scalar.toArcMinute(a))
let toArcSecond = a => mapScalarElements(a, a => Scalar.toArcSecond(a))
let toGrad = a => mapScalarElements(a, a => Scalar.toGrad(a))

let re = a => mapScalarElements(a, a => Scalar.re(a))
let im = a => mapScalarElements(a, a => Scalar.im(a))
let conj = a => mapScalarElements(a, a => Scalar.conj(a))

%%private(
  let map2Scalar = (a: t, b: t, fn: (Scalar.t, Scalar.t) => Scalar.t): t =>
    switch (a, b) {
    | (#...Scalar.t as a, #...Scalar.t as b) => fn(a, b)->ofScalar
    | _ => nan
    }
)

let logBase = (~base, a) => map2Scalar(base, a, (base, a) => Scalar.logBase(~base, a))
let rem = (a, b) => map2Scalar(a, b, (a, b) => Scalar.rem(a, b))
let max = (a, b) => map2Scalar(a, b, (a, b) => Scalar.max(a, b))
let min = (a, b) => map2Scalar(a, b, (a, b) => Scalar.min(a, b))
let gcd = (a, b) => map2Scalar(a, b, (a, b) => Scalar.gcd(a, b))
let lcm = (a, b) => map2Scalar(a, b, (a, b) => Scalar.lcm(a, b))

let transpose = (a: t) =>
  switch a {
  | #...Scalar.t as a => a
  | #Matx(m) => Matrix.transpose(m)->ofMatrix
  | #Vect(v) => Matrix.make(~numRows=1, ~numColumns=Vector.size(v), Vector.elements(v))->ofMatrix
  | _ => nan
  }

let trace = (a: t) =>
  switch a {
  | #...Scalar.t as a => a
  | #Matx(m) => Matrix.trace(m)->ofScalar
  | _ => nan
  }

let rref = (a: t) =>
  switch a {
  | #...Scalar.t => ofScalar(Scalar.one)
  | #Matx(m) => Matrix.rref(m)->ofMatrix
  | #Vect(v) =>
    Matrix.makeBy(~numRows=1, ~numColumns=Vector.size(v), (row, _) =>
      row == 0 ? Scalar.one : Scalar.zero
    )->ofMatrix
  | _ => nan
  }
