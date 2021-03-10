type t = list<(int, int)>

let empty: t = list{}

let addSequentialIndex = (x, i) =>
  switch x {
  | list{(a, b), ...rest} if b == i - 1 => list{(a, i), ...rest}
  | rest => list{(i, i), ...rest}
  }

let contains = (x: t, i: int) => Belt.List.some(x, ((a, b)) => a <= i && b >= i)

let toArray = (x: t) => Belt.List.toArray(x)
