type t = list((string, Value.t));

let empty = [];

let rec get = (x: t, key) =>
  switch (x) {
  | [(k, v), ..._] when k == key => Some(v)
  | [_, ...rest] => get(rest, key)
  | [] => None
  };

let rec set = (x: t, key, value) =>
  switch (x) {
  | [(k, _), ...rest] when k == key => [(key, value), ...rest]
  | [tuple, ...rest] => [tuple, ...set(rest, key, value)]
  | [] => [(key, value)]
  };
