let maxInt = Js.Int.max

let abs = x => Js.Math.abs_int(x)->lor(0)

let safeMod = (a, b) => {
  // https://stackoverflow.com/a/17323608
  open SafeInt
  let a = ofInt(a)
  let b = ofInt(b)
  mod(mod(a, b) + b, b)->toInt
}

let rec gcd = (a, b) => b == 0 ? a : gcd(b, mod(a, b))
