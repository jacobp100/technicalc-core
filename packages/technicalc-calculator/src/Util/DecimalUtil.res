open Decimal

let magnitude = (~base, f) =>
  if f != zero {
    abs(f)->(logBase(_, ofInt(base)))->floor
  } else {
    zero
  }

type bounds =
  | @as(0) BothBound
  | @as(1) LowerBound
  | @as(2) UpperBound
  | @as(3) Inside
  | @as(4) Outside

let bounds = (~lower=?, ~upper=?, f) => {
  let lowerCompare = switch lower {
  | Some(l) => cmp(l, f)
  | None => -1
  }
  let upperCompare = switch upper {
  | Some(u) => cmp(u, f)
  | None => 1
  }
  switch (lowerCompare, upperCompare) {
  | (0, 0) => BothBound
  | (0, _) => LowerBound
  | (_, 0) => UpperBound
  | (-1, 1) => Inside
  | _ => Outside
  }
}
