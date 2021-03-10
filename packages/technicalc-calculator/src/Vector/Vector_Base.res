open Vector_Types

let empty: t = []
let isEmpty = (x: t) => Belt.Array.length(x) == 0

let equal = (a: t, b: t) =>
  Belt.Array.length(a) == Belt.Array.length(b) && Belt.Array.every2(a, b, Scalar.equal)

let size: t => int = Belt.Array.length
let makeByU: (int, (. int) => Scalar.t) => t = Belt.Array.makeByU
let getExn: (t, int) => Scalar.t = Belt.Array.getExn
let map: (t, Scalar.t => Scalar.t) => t = Belt.Array.map
let mapU: (t, (. Scalar.t) => Scalar.t) => t = Belt.Array.mapU
let some: (t, Scalar.t => bool) => bool = Belt.Array.some
