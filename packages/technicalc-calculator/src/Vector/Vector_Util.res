open Vector_Types
open Vector_Base

let scalarElements = (x: t): array<Scalar.t> => Obj.magic(x)

let zipBy = (x: t, y: t, fn: (Scalar.t, Scalar.t) => Scalar.t): t => {
  if Belt.Array.length(x) == Belt.Array.length(y) {
    makeByU(Belt.Array.length(x), (. i) => {
      fn(
        Belt.Array.getUnsafe(x, i)->Scalar.Finite.toScalar,
        Belt.Array.getUnsafe(y, i)->Scalar.Finite.toScalar,
      )
    })
  } else {
    empty
  }
}

let some: (t, Scalar.t => bool) => bool = Obj.magic(Belt.Array.some)
