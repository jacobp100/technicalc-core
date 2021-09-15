open Vector_Types
open Vector_Base

let neg = (a: t): t => map(a, Scalar.neg)

let magnitudeSquared = (a: t): Scalar.t =>
  elements(a)->Belt.Array.reduceU(Scalar.zero, (. a, s) => {
    open Scalar
    a + s * s
  })

let dot = (a: t, b: t): Scalar.t =>
  if size(a) == size(b) {
    let out = ref(Scalar.zero)

    let aElements = elements(a)
    let bElements = elements(b)

    for i in 0 to size(a) - 1 {
      open Scalar_Operators
      out := out.contents + Belt.Array.getUnsafe(aElements, i) * Belt.Array.getUnsafe(bElements, i)
    }

    out.contents
  } else {
    Scalar.nan
  }
