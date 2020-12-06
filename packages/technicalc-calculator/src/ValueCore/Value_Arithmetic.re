open Value_Types;
open Value_Base;

let%private hundredS: Scalar.t = `R(Real.ofInt(100));

let neg = (a: t) =>
  switch (a) {
  | #Scalar.t as s => Scalar.neg(s)->ofScalar
  | `P(p) => Scalar.neg(p)->ofPercent
  | `V(v) => Vector.neg(v)->ofVector
  | `M(m) => Matrix.neg(m)->ofMatrix
  | `N => `N
  };

let add = (a: t, b: t) =>
  switch (a, b) {
  | (#Scalar.t as aS, #Scalar.t as bS) => Scalar.add(aS, bS)->ofScalar
  | (#Scalar.t as s, `P(p)) => Scalar.(s + s * p / hundredS)->ofScalar
  | (`V(aV), `V(bV)) => Vector.add(aV, bV)->ofVector
  | (`M(aM), `M(bM)) => Matrix.add(aM, bM)->ofMatrix
  | _ => `N
  };

let sub = (a: t, b: t) =>
  switch (a, b) {
  | (#Scalar.t as aS, #Scalar.t as bS) => Scalar.sub(aS, bS)->ofScalar
  | (#Scalar.t as s, `P(p)) => Scalar.(s - s * p / hundredS)->ofScalar
  | (`V(aV), `V(bV)) => Vector.sub(aV, bV)->ofVector
  | (`M(aM), `M(bM)) => Matrix.sub(aM, bM)->ofMatrix
  | _ => `N
  };

let mul = (a: t, b: t) =>
  switch (a, b) {
  | (#Scalar.t as aS, #Scalar.t as bS) => Scalar.mul(aS, bS)->ofScalar
  | (#Scalar.t as s, `P(p))
  | (`P(p), #Scalar.t as s) => Scalar.(p * s / hundredS)->ofScalar
  | (`V(aV), `V(bV)) => Vector.mul(aV, bV)->ofVector
  | (`M(aM), `M(bM)) => Matrix.mul(aM, bM)->ofMatrix
  | (`V(v), #Scalar.t as s)
  | (#Scalar.t as s, `V(v)) => Vector.mulScalar(v, s)->ofVector
  | (`M(m), #Scalar.t as s)
  | (#Scalar.t as s, `M(m)) => Matrix.mulScalar(m, s)->ofMatrix
  | (`M(m), `V(v)) => Matrix.mulVector(m, v)->ofVector
  | _ => `N
  };

let div = (a: t, b: t) =>
  switch (a, b) {
  | (#Scalar.t as aS, #Scalar.t as bS) => Scalar.div(aS, bS)->ofScalar
  | (#Scalar.t as s, `P(p)) => Scalar.(s / (one + p / hundredS))->ofScalar
  | (`V(v), #Scalar.t as s) => Vector.divScalar(v, s)->ofVector
  | (`M(m), #Scalar.t as s) => Matrix.divScalar(m, s)->ofMatrix
  | _ => `N
  };
