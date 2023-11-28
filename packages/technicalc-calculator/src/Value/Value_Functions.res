open Value_Types
open Value_Base

%%private(
  let mapScalarU = (a: t, fn: (. Scalar.t) => Scalar.t): t =>
    switch a {
    | #...Scalar.t as t => fn(. t)->ofScalar
    | _ => nan
    }
)

%%private(
  let mapScalarElementsU = (a: t, fn: (. Scalar.t) => Scalar.t): t =>
    switch a {
    | #...Scalar.t as t => fn(. t)->ofScalar
    | #Pcnt(p) =>
      let p = Scalar.Finite.toScalar(p)
      fn(. p)->ofPercent
    | #Matx(m) => Matrix.mapU(m, fn)->ofMatrix
    | #Vect(v) => Vector.mapU(v, fn)->ofVector
    | #Mesr({value, units}) =>
      Measure.ofReal(Scalar.ofReal(value)->fn(. _)->Scalar.toReal, ~units)->ofMeasure
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

let sqrt = a => mapScalarU(a, (. a) => Scalar.sqrt(a))
let log = a => mapScalarU(a, (. a) => Scalar.log(a))
let exp = a => mapScalarU(a, (. a) => Scalar.exp(a))

let round = a => mapScalarElementsU(a, (. a) => Scalar.round(a))
let floor = a => mapScalarElementsU(a, (. a) => Scalar.floor(a))
let ceil = a => mapScalarElementsU(a, (. a) => Scalar.ceil(a))

let ofDeg = a => mapScalarElementsU(a, (. a) => Scalar.ofDeg(a))
let ofArcMin = a => mapScalarElementsU(a, (. a) => Scalar.ofArcMin(a))
let ofArcSec = a => mapScalarElementsU(a, (. a) => Scalar.ofArcSec(a))
let ofGrad = a => mapScalarElementsU(a, (. a) => Scalar.ofGrad(a))

let toDeg = a => mapScalarElementsU(a, (. a) => Scalar.toDeg(a))
let toArcMinute = a => mapScalarElementsU(a, (. a) => Scalar.toArcMinute(a))
let toArcSecond = a => mapScalarElementsU(a, (. a) => Scalar.toArcSecond(a))
let toGrad = a => mapScalarElementsU(a, (. a) => Scalar.toGrad(a))

let re = a => mapScalarElementsU(a, (. a) => Scalar.re(a))
let im = a => mapScalarElementsU(a, (. a) => Scalar.im(a))
let conj = a => mapScalarElementsU(a, (. a) => Scalar.conj(a))

%%private(
  let map2ScalarU = (a: t, b: t, fn: (. Scalar.t, Scalar.t) => Scalar.t): t =>
    switch (a, b) {
    | (#...Scalar.t as a, #...Scalar.t as b) => fn(. a, b)->ofScalar
    | _ => nan
    }
)

let logBase = (~base, a) => map2ScalarU(base, a, (. base, a) => Scalar.logBase(~base, a))
let rem = (a, b) => map2ScalarU(a, b, (. a, b) => Scalar.rem(a, b))
let max = (a, b) => map2ScalarU(a, b, (. a, b) => Scalar.max(a, b))
let min = (a, b) => map2ScalarU(a, b, (. a, b) => Scalar.min(a, b))
let gcd = (a, b) => map2ScalarU(a, b, (. a, b) => Scalar.gcd(a, b))
let lcm = (a, b) => map2ScalarU(a, b, (. a, b) => Scalar.lcm(a, b))

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
    Matrix.makeByU(~numRows=1, ~numColumns=Vector.size(v), (. row, _) =>
      row == 0 ? Scalar.one : Scalar.zero
    )->ofMatrix
  | _ => nan
  }
