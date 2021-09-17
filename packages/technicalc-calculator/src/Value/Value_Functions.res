open Value_Types
open Value_Base

%%private(
  let mapScalarU = (a: t, fn: (. Scalar.t) => Scalar.t): t =>
    switch a {
    | #...Scalar.t as t => fn(. t)->ofScalar
    | #Pcnt(p) =>
      let p = Scalar.Finite.toScalar(p)
      fn(. p)->ofPercent
    | #Matx(m) => Matrix.mapU(m, fn)->ofMatrix
    | #Vect(v) => Vector.mapU(v, fn)->ofVector
    }
)

let dot = (a: t, b: t): t =>
  switch (a, b) {
  | (#Vect(aV), #Vect(bV)) => Vector.dot(aV, bV)->ofScalar
  | _ => Value_Arithmetic.mul(a, b)
  }

let inv = a => mapScalarU(a, (. a) => Scalar.inv(a))

let abs = (a: t): t =>
  switch a {
  | #Zero => zero // Optimisation to avoid extra function calls
  | #...Scalar.t as t => Scalar.abs(t)->ofScalar
  | #Pcnt(p) =>
    let p = Scalar.Finite.toScalar(p)
    Scalar.abs(p)->ofPercent
  | #Matx(m) => Matrix.determinant(m)->ofScalar
  | #Vect(v) => Vector.magnitudeSquared(v)->Scalar.sqrt->ofScalar
  }

let sqrt = a => mapScalarU(a, (. a) => Scalar.sqrt(a))
let log = a => mapScalarU(a, (. a) => Scalar.log(a))
let exp = a => mapScalarU(a, (. a) => Scalar.exp(a))

let round = a => mapScalarU(a, (. a) => Scalar.round(a))
let floor = a => mapScalarU(a, (. a) => Scalar.floor(a))
let ceil = a => mapScalarU(a, (. a) => Scalar.ceil(a))

let ofDeg = a => mapScalarU(a, (. a) => Scalar.ofDeg(a))
let ofArcMin = a => mapScalarU(a, (. a) => Scalar.ofArcMin(a))
let ofArcSec = a => mapScalarU(a, (. a) => Scalar.ofArcSec(a))
let ofGrad = a => mapScalarU(a, (. a) => Scalar.ofGrad(a))

let toDeg = a => mapScalarU(a, (. a) => Scalar.toDeg(a))
let toArcMinute = a => mapScalarU(a, (. a) => Scalar.toArcMinute(a))
let toArcSecond = a => mapScalarU(a, (. a) => Scalar.toArcSecond(a))
let toGrad = a => mapScalarU(a, (. a) => Scalar.toGrad(a))

let re = a => mapScalarU(a, (. a) => Scalar.re(a))
let im = a => mapScalarU(a, (. a) => Scalar.im(a))
let conj = a => mapScalarU(a, (. a) => Scalar.conj(a))

%%private(
  let map2ScalarU = (a: t, b: t, fn: (. Scalar.t, Scalar.t) => Scalar.t): t =>
    switch (a, b) {
    | (#...Scalar.t as a, #...Scalar.t as b) => fn(. a, b)->ofScalar
    | _ => nan
    }
)

let max = (a, b) => map2ScalarU(a, b, (. a, b) => Scalar.max(a, b))
let min = (a, b) => map2ScalarU(a, b, (. a, b) => Scalar.min(a, b))
let gcd = (a, b) => map2ScalarU(a, b, (. a, b) => Scalar.gcd(a, b))
let lcm = (a, b) => map2ScalarU(a, b, (. a, b) => Scalar.lcm(a, b))
