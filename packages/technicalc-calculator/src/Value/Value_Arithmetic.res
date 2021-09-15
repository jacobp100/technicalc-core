open Value_Types
open Value_Base

%%private(let percentToNumerical = Value_Util.percentToNumerical)

let neg = (a: t) =>
  switch a {
  | #...Scalar.t as s => Scalar.neg(s)->ofScalar
  | #Pcnt(p) => Scalar.neg(Scalar.Finite.toScalar(p))->ofPercent
  | #Vect(v) => Vector.neg(v)->ofVector
  | #Matx(m) => Matrix.neg(m)->ofMatrix
  }

let add = (a: t, b: t) =>
  switch (a, b) {
  | (#...Scalar.t as aS, #...Scalar.t as bS) => Scalar.add(aS, bS)->ofScalar
  | (#...Scalar.t as s, #Pcnt(p)) =>
    open Scalar
    ofScalar(s + s * percentToNumerical(p))
  | (#Vect(aV), #Vect(bV)) => Vector.add(aV, bV)->ofVector
  | (#Matx(aM), #Matx(bM)) => Matrix.add(aM, bM)->ofMatrix
  | _ => nan
  }

let sub = (a: t, b: t) =>
  switch (a, b) {
  | (#...Scalar.t as aS, #...Scalar.t as bS) => Scalar.sub(aS, bS)->ofScalar
  | (#...Scalar.t as s, #Pcnt(p)) =>
    open Scalar
    ofScalar(s - s * percentToNumerical(p))
  | (#Vect(aV), #Vect(bV)) => Vector.sub(aV, bV)->ofVector
  | (#Matx(aM), #Matx(bM)) => Matrix.sub(aM, bM)->ofMatrix
  | _ => nan
  }

let mul = (a: t, b: t) =>
  switch (a, b) {
  | (#...Scalar.t as aS, #...Scalar.t as bS) => Scalar.mul(aS, bS)->ofScalar
  | (#...Scalar.t as s, #Pcnt(p))
  | (#Pcnt(p), #...Scalar.t as s) =>
    open Scalar
    ofScalar(s * percentToNumerical(p))
  | (#Vect(aV), #Vect(bV)) => Vector.mul(aV, bV)->ofVector
  | (#Matx(aM), #Matx(bM)) => Matrix.mul(aM, bM)->ofMatrix
  | (#Vect(v), #...Scalar.t as s)
  | (#...Scalar.t as s, #Vect(v)) =>
    Vector.mulScalar(v, s)->ofVector
  | (#Matx(m), #...Scalar.t as s)
  | (#...Scalar.t as s, #Matx(m)) =>
    Matrix.mulScalar(m, s)->ofMatrix
  | (#Matx(m), #Vect(v)) => Matrix.mulVector(m, v)->ofVector
  | _ => nan
  }

let div = (a: t, b: t) =>
  switch (a, b) {
  | (#...Scalar.t as aS, #...Scalar.t as bS) => Scalar.div(aS, bS)->ofScalar
  | (#...Scalar.t as s, #Pcnt(p)) =>
    open Scalar
    ofScalar(s / (one + percentToNumerical(p)))
  | (#Vect(v), #...Scalar.t as s) => Vector.divScalar(v, s)->ofVector
  | (#Matx(m), #...Scalar.t as s) => Matrix.divScalar(m, s)->ofMatrix
  | _ => nan
  }
