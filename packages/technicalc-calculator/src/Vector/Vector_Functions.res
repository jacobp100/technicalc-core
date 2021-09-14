open Vector_Types
open Vector_Base

let neg = (a: t): t => map(a, Scalar.neg)

let magnitudeSquared = (a: t): Scalar.t =>
  Belt.Array.reduceU(a, Scalar.zero, (. a, s) => {
    open Scalar
    let s = Finite.toScalar(s)
    a + s * s
  })

let dot = (a: t, b: t): Scalar.t =>
  if Belt.Array.length(a) == Belt.Array.length(b) {
    let out = ref(Scalar.zero)

    for i in 0 to Belt.Array.length(a) - 1 {
      open Scalar_Operators
      out :=
        out.contents +
        Belt.Array.getUnsafe(a, i)->Scalar.Finite.toScalar *
          Belt.Array.getUnsafe(b, i)->Scalar.Finite.toScalar
    }

    out.contents
  } else {
    Scalar.nan
  }
