open Value_Types;
open Value_Base;

let neg = (a: t) =>
  switch (a) {
  | #Scalar.t as s => Scalar.neg(s)->ofScalar
  | `P(p) => Scalar.neg(p)->ofPercent
  | `V(v) => Vector.neg(v)->ofVector
  | `M(m) => Matrix.neg(m)->ofMatrix
  | `N => `N
  };
