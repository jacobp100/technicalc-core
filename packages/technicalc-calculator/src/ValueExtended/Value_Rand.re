open Value_Base;

let rand = () => Random.float(1.)->ofFloat;

let randInt = (a, b) =>
  switch (a->toInt, b->toInt) {
  | (Some(a), Some(b)) => (min(a, b) + Random.int(abs(b - a)))->ofInt
  | _ => nan
  };
