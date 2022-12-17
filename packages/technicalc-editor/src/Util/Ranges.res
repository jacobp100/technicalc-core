type t = list<(int, int)>

let empty: t = list{}

let contains = (x: t, i: int) => Belt.List.someU(x, (. (a, b)) => a <= i && b >= i)

let toArray = (x: t) => Belt.List.toArray(x)
