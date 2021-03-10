type t = list<(string, Value.t)>

let empty = list{}

let rec get = (x: t, key) =>
  switch x {
  | list{(k, v), ..._} if k == key => Some(v)
  | list{_, ...rest} => get(rest, key)
  | list{} => None
  }

let rec set = (x: t, key, value) =>
  switch x {
  | list{(k, _), ...rest} if k == key => list{(key, value), ...rest}
  | list{tuple, ...rest} => list{tuple, ...set(rest, key, value)}
  | list{} => list{(key, value)}
  }
