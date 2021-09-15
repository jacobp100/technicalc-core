open Vector_Types

@get external size: t => int = "length"

let empty: t = []
let isEmpty = (x: t) => size(x) == 0

let eq = (a: t, b: t) => size(a) == size(b) && Belt.Array.every2(a, b, Obj.magic(Scalar_Base.eq))

let make = (elements: array<Scalar.t>): t =>
  !Belt.Array.some(elements, Scalar_Base.isNaN) ? Obj.magic(elements) : empty

let makeByU = (size: int, fn: (. int) => Scalar.t): t => {
  let elements = Belt.Array.makeUninitializedUnsafe(size)

  let rec iter = i =>
    if i < size {
      switch fn(. i)->Scalar_Finite.ofScalar {
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

external elements: t => array<Scalar.t> = "%identity"

let getExn: (t, int) => Scalar.t = Obj.magic(Belt.Array.getExn)

let map = (x: t, fn: Scalar.t => Scalar.t): t =>
  makeByU(size(x), (. i) => {
    fn(Belt.Array.getUnsafe(x, i)->Scalar_Finite.toScalar)
  })
