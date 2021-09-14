open Vector_Types

let empty: t = []
let isEmpty = (x: t) => Belt.Array.length(x) == 0

let eq = (a: t, b: t) =>
  Belt.Array.length(a) == Belt.Array.length(b) && Belt.Array.every2(a, b, Obj.magic(Scalar.eq))

let size: t => int = Belt.Array.length

let make = (elements: array<Scalar.t>): t =>
  !Belt.Array.some(elements, Scalar.isNaN) ? Obj.magic(elements) : empty

let makeByU = (size: int, fn: (. int) => Scalar.t): t => {
  let elements = Belt.Array.makeUninitializedUnsafe(size)

  let rec iter = i =>
    if i < size {
      switch fn(. i)->Scalar.Finite.ofScalar {
      | Some(x) =>
        Belt.Array.setUnsafe(elements, i, x)
        iter(i + 1)
      | None => empty
      }
    } else {
      elements
    }

  iter(0)
}

let map = (x: t, fn: Scalar.t => Scalar.t): t =>
  makeByU(Belt.Array.length(x), (. i) => {
    fn(Belt.Array.getUnsafe(x, i)->Scalar.Finite.toScalar)
  })

let getExn: (t, int) => Scalar.finite = Belt.Array.getExn
