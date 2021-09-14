open Value_Types
open Value_Base

%%private(let hundredS = Scalar.ofReal(Real.ofInt(100)))

let neg = (a: t) =>
  switch a {
  | #...Scalar.t as s => Scalar.neg(s)->ofScalar
  | #P(p) => Scalar.neg(Scalar.Finite.toScalar(p))->ofPercent
  | #V(v) => Vector.neg(v)->ofVector
  | #M(m) => Matrix.neg(m)->ofMatrix
  }

let add = (a: t, b: t) =>
  switch (a, b) {
  | (#...Scalar.t as aS, #...Scalar.t as bS) => Scalar.add(aS, bS)->ofScalar
  | (#...Scalar.t as s, #P(p)) =>
    open Scalar
    let p = Finite.toScalar(p)
    ofScalar(s + s * p / hundredS)
  | (#V(aV), #V(bV)) => Vector.add(aV, bV)->ofVector
  | (#M(aM), #M(bM)) => Matrix.add(aM, bM)->ofMatrix
  | _ => nan
  }

let sub = (a: t, b: t) =>
  switch (a, b) {
  | (#...Scalar.t as aS, #...Scalar.t as bS) => Scalar.sub(aS, bS)->ofScalar
  | (#...Scalar.t as s, #P(p)) =>
    open Scalar
    let p = Finite.toScalar(p)
    ofScalar(s - s * p / hundredS)
  | (#V(aV), #V(bV)) => Vector.sub(aV, bV)->ofVector
  | (#M(aM), #M(bM)) => Matrix.sub(aM, bM)->ofMatrix
  | _ => nan
  }

let mul = (a: t, b: t) =>
  switch (a, b) {
  | (#...Scalar.t as aS, #...Scalar.t as bS) => Scalar.mul(aS, bS)->ofScalar
  | (#...Scalar.t as s, #P(p))
  | (#P(p), #...Scalar.t as s) =>
    open Scalar
    let p = Finite.toScalar(p)
    ofScalar(p * s / hundredS)
  | (#V(aV), #V(bV)) => Vector.mul(aV, bV)->ofVector
  | (#M(aM), #M(bM)) => Matrix.mul(aM, bM)->ofMatrix
  | (#V(v), #...Scalar.t as s)
  | (#...Scalar.t as s, #V(v)) =>
    Vector.mulScalar(v, s)->ofVector
  | (#M(m), #...Scalar.t as s)
  | (#...Scalar.t as s, #M(m)) =>
    Matrix.mulScalar(m, s)->ofMatrix
  | (#M(m), #V(v)) => Matrix.mulVector(m, v)->ofVector
  | _ => nan
  }

let div = (a: t, b: t) =>
  switch (a, b) {
  | (#...Scalar.t as aS, #...Scalar.t as bS) => Scalar.div(aS, bS)->ofScalar
  | (#...Scalar.t as s, #P(p)) =>
    open Scalar
    let p = Finite.toScalar(p)
    ofScalar(s / (one + p / hundredS))
  | (#V(v), #...Scalar.t as s) => Vector.divScalar(v, s)->ofVector
  | (#M(m), #...Scalar.t as s) => Matrix.divScalar(m, s)->ofMatrix
  | _ => nan
  }
