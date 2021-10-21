open Value_Base

let rand = () => FloatUtil.random()->ofFloat

let randInt = (a, b) => {
  let int = switch (a->toInt, b->toInt) {
  | (Some(a), Some(b)) =>
    floor(
      min(a, b)->Belt.Float.fromInt +.
        FloatUtil.random() *. Belt.Float.fromInt(b - a)->FloatUtil.abs,
    )->FloatUtil.toInt
  | _ => None
  }
  switch int {
  | Some(i) => ofInt(i)
  | None => nan
  }
}
