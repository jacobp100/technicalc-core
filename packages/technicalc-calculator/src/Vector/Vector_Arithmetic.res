open Vector_Types
open Vector_Base
open Vector_Util

let add = (a: t, b: t): t => zipBy(a, b, Scalar.add)

let sub = (a: t, b: t): t => zipBy(a, b, Scalar.sub)

let mul = (a: t, b: t): t =>
  switch (scalarElements(a), scalarElements(b)) {
  | ([a1, a2, a3], [b1, b2, b3]) =>
    open Scalar
    make([a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1])
  | _ => empty
  }
