type t = list((int, int));

let empty: t = [];

let addSequentialIndex = (x, i) =>
  switch (x) {
  | [(a, b), ...rest] when b == i - 1 => [(a, i), ...rest]
  | rest => [(i, i), ...rest]
  };

let contains = (x: t, i: int) =>
  Belt.List.some(x, ((a, b)) => a <= i && b >= i);

let toArray = (x: t) => Belt.List.toArray(x);
