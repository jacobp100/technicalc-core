open Vector_Types
open Vector_Base

let add = (a: t, b: t): t =>
  Belt.Array.length(a) == Belt.Array.length(b) ? Belt.Array.zipBy(a, b, Scalar.add) : empty

let sub = (a: t, b: t): t =>
  Belt.Array.length(a) == Belt.Array.length(b) ? Belt.Array.zipBy(a, b, Scalar.sub) : empty

let mul = (a: t, b: t): t =>
  switch (a, b) {
  | ([a1, a2, a3], [b1, b2, b3]) =>
    open Scalar
    [a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1]
  | _ => empty
  }
