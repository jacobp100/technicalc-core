open Vector_Types

@get external size: t => int = "length"

let empty: t = []
let isEmpty = (x: t) => size(x) == 0

let xUnit: t = [#Real(Real.one), #Zero, #Zero]
let yUnit: t = [#Zero, #Real(Real.one), #Zero]
let zUnit: t = [#Zero, #Zero, #Real(Real.one)]

let eq = (a: t, b: t) =>
  size(a) == size(b) &&
    Belt.Array.every2U(a, b, (. a, b) => {
      Scalar_Base.eq(Scalar.Finite.toScalar(a), Scalar.Finite.toScalar(b))
    })

let make = (elements: array<Scalar.t>): t =>
  !Belt.Array.someU(elements, (. element) => Scalar_Base.isNaN(element))
    ? Obj.magic(elements)
    : empty

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

let mapU = (x: t, fn: (. Scalar.t) => Scalar.t): t =>
  makeByU(size(x), (. i) => {
    fn(. Belt.Array.getUnsafe(x, i)->Scalar_Finite.toScalar)
  })
