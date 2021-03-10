open Value_Core

%%private(
  let reduceRange = (a, b, initialValue, f, iteratee) =>
    switch (toInt(a), toInt(b)) {
    | (Some(a), Some(b)) if b >= a =>
      let current = ref(initialValue)
      for i in a to b {
        current := iteratee(current.contents, f(ofInt(i)))
      }
      current.contents
    | _ => nan
    }
)

let sum = (f, a, b) => reduceRange(a, b, zero, f, add)
let product = (f, a, b) => reduceRange(a, b, one, f, mul)
