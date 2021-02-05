open Decimal;

let magnitude = f =>
  if (f != zero) {
    abs(f)->log10->floor->toFloat->FloatUtil.intValueExn;
  } else {
    0;
  };

type bounds =
  | BothBound
  | LowerBound
  | UpperBound
  | Inside
  | Outside;

let bounds = (~lower=?, ~upper=?, f) => {
  let lowerCompare =
    switch (lower) {
    | Some(l) => cmp(l, f)
    | None => (-1)
    };
  let upperCompare =
    switch (upper) {
    | Some(u) => cmp(u, f)
    | None => 1
    };
  switch (lowerCompare, upperCompare) {
  | (0, 0) => BothBound
  | (0, _) => LowerBound
  | (_, 0) => UpperBound
  | ((-1), 1) => Inside
  | _ => Outside
  };
};
