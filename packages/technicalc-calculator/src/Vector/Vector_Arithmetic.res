open Vector_Types
open Vector_Base

%%private(
  let zipBy = (a: t, b: t, fn: (Scalar.t, Scalar.t) => Scalar.t): t =>
    if size(a) == size(b) {
      let aElements = elements(a)
      let bElements = elements(b)

      makeByU(size(a), (. i) => {
        fn(Belt.Array.getUnsafe(aElements, i), Belt.Array.getUnsafe(bElements, i))
      })
    } else {
      empty
    }
)

let add = (a: t, b: t): t => zipBy(a, b, Scalar.add)

let sub = (a: t, b: t): t => zipBy(a, b, Scalar.sub)

let mul = (a: t, b: t): t =>
  switch (elements(a), elements(b)) {
  | ([a1, a2, a3], [b1, b2, b3]) =>
    open Scalar
    make([a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1])
  | _ => empty
  }
