open Value_Types;
open Value_Base;

let dot = (a: t, b: t): t =>
  switch (a, b) {
  | (`V(aV), `V(bV)) => Vector.dot(aV, bV)->ofScalar
  | _ => Value_Arithmetic.mul(a, b)
  };

let abs = (a: t): t =>
  switch (a) {
  | #Scalar.t as t => Scalar.abs(t)->ofScalar
  | `P(p) => Scalar.abs(p)->ofPercent
  | `M(m) => Matrix.determinant(m)->ofScalar
  | `V(v) => Vector.magnitudeSquared(v)->ofScalar->Value_Powers.sqrt
  | `N => `N
  };

let%private mapScalar = (a: t, fn: Scalar.t => Scalar.t): t =>
  switch (a) {
  | #Scalar.t as t => fn(t)->ofScalar
  | `P(p) => fn(p)->ofPercent
  | `M(m) => Matrix.map(m, fn)->ofMatrix
  | `V(v) => Vector.map(v, fn)->ofVector
  | `N => `N
  };

let round = a => mapScalar(a, Scalar.round);
let floor = a => mapScalar(a, Scalar.floor);
let ceil = a => mapScalar(a, Scalar.ceil);

let deg = a => mapScalar(a, Scalar.deg);
let grad = a => mapScalar(a, Scalar.grad);

let%private map2Scalar = (a: t, b: t, fn: (Scalar.t, Scalar.t) => Scalar.t): t =>
  switch (a, b) {
  | (#Scalar.t as a, #Scalar.t as b) => fn(a, b)->ofScalar
  | _ => `N
  };

let max = (a, b) => map2Scalar(a, b, Scalar.max);
let min = (a, b) => map2Scalar(a, b, Scalar.min);
let gcd = (a, b) => map2Scalar(a, b, Scalar.gcd);
let lcm = (a, b) => map2Scalar(a, b, Scalar.lcm);
