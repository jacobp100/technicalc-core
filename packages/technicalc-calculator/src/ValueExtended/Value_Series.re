open Value_Core;

let%private reduceRange = (a, b, initialValue, f, iteratee) =>
  switch (toInt(a), toInt(b)) {
  | (Some(a), Some(b)) when b >= a =>
    let current = ref(initialValue);
    for (i in a to b) {
      current := iteratee(current^, f(ofInt(i)));
    };
    current^;
  | _ => nan
  };

let sum = (f, a, b) => reduceRange(a, b, zero, f, add);
let product = (f, a, b) => reduceRange(a, b, one, f, mul);
