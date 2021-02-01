open Value_Base;

let rand = () => FloatUtil.random()->ofFloat;

let randInt = (a, b) => {
  let int =
    switch (a->toInt, b->toInt) {
    | (Some(a), Some(b)) =>
      FloatUtil.toInt(
        min(a, b)->Belt.Float.fromInt
        +. FloatUtil.random()
        *. Belt.Float.fromInt(b - a)->FloatUtil.abs,
      )
    | _ => None
    };
  switch (int) {
  | Some(i) => ofInt(i)
  | None => nan
  };
};
