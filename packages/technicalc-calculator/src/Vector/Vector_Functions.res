open Vector_Types
open Vector_Base

let magnitudeSquared = (a: t): Scalar.t =>
  elements(a)->Belt.Array.reduceU(Scalar.zero, (. a, s) => {
    open Scalar
    a + s * s
  })
