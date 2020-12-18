open Value_Base;

let rand = () => FloatUtil.random()->ofFloat;

let randInt = (a, b) => {
  let int =
    switch (a->toInt, b->toInt) {
    | (Some(a), Some(b)) =>
      FloatUtil.toInt(
        min(a, b)->float_of_int
        +. FloatUtil.random()
        *. float_of_int(b - a)->abs_float,
      )
    | _ => None
    };
  switch (int) {
  | Some(i) => ofInt(i)
  | None => nan
  };
};
