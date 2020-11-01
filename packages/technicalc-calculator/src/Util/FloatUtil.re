// Checks int boundaries too
let isInt = f => f->int_of_float->float_of_int == f;

// Converts to an int with no rounding, or returns None.
// E.g. 2 -> Some(2), 2.5 -> None
let intValue = f =>
  f->int_of_float->float_of_int == f ? Some(f->int_of_float) : None;

let intValueExn = f => {
  assert(f->int_of_float->float_of_int == f);
  int_of_float(f);
};

// Only used in one place, don't bother inlining stuff for perf here
let toInt = f => floor(f)->intValue;

let isFinite = f =>
  switch (classify_float(f)) {
  | FP_zero
  | FP_normal
  | FP_subnormal => true
  | FP_infinite
  | FP_nan => false
  };
