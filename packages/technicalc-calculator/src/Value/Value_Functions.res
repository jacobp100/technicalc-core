open Value_Types
open Value_Base

%%private(
  let mapScalar = (a: t, fn: Scalar.t => Scalar.t): t =>
    switch a {
    | #...Scalar.t as t => fn(t)->ofScalar
    | #Pcnt(p) =>
      let p = Scalar.Finite.toScalar(p)
      fn(p)->ofPercent
    | #Matx(m) => Matrix.map(m, fn)->ofMatrix
    | #Vect(v) => Vector.map(v, fn)->ofVector
    }
)

let dot = (a: t, b: t): t =>
  switch (a, b) {
  | (#Vect(aV), #Vect(bV)) => Vector.dot(aV, bV)->ofScalar
  | _ => Value_Arithmetic.mul(a, b)
  }

let inv = a => mapScalar(a, Scalar.inv)

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

let sqrt = a => mapScalar(a, Scalar.sqrt)
let log = a => mapScalar(a, Scalar.log)
let exp = a => mapScalar(a, Scalar.exp)

let round = a => mapScalar(a, Scalar.round)
let floor = a => mapScalar(a, Scalar.floor)
let ceil = a => mapScalar(a, Scalar.ceil)

let ofDeg = a => mapScalar(a, Scalar.ofDeg)
let ofArcMin = a => mapScalar(a, Scalar.ofArcMin)
let ofArcSec = a => mapScalar(a, Scalar.ofArcSec)
let ofGrad = a => mapScalar(a, Scalar.ofGrad)

let toDeg = a => mapScalar(a, Scalar.toDeg)
let toArcMinute = a => mapScalar(a, Scalar.toArcMinute)
let toArcSecond = a => mapScalar(a, Scalar.toArcSecond)
let toGrad = a => mapScalar(a, Scalar.toGrad)

let re = a => mapScalar(a, Scalar.re)
let im = a => mapScalar(a, Scalar.im)
let conj = a => mapScalar(a, Scalar.conj)

%%private(
  let map2Scalar = (a: t, b: t, fn: (Scalar.t, Scalar.t) => Scalar.t): t =>
    switch (a, b) {
    | (#...Scalar.t as a, #...Scalar.t as b) => fn(a, b)->ofScalar
    | _ => nan
    }
)

let max = (a, b) => map2Scalar(a, b, Scalar.max)
let min = (a, b) => map2Scalar(a, b, Scalar.min)
let gcd = (a, b) => map2Scalar(a, b, Scalar.gcd)
let lcm = (a, b) => map2Scalar(a, b, Scalar.lcm)
