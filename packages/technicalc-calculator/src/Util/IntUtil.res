let maxInt = Js.Int.max

let abs = x => Js.Math.abs_int(x)->lor(0)

let safeMod = (a, b) => {
  let modAB = mod(a, b)
  switch SafeInt.addInt(modAB, b) {
  | Some(a) => mod(a, b)
  | None => modAB
  }
}

let rec gcd = (a, b) => b == 0 ? a : gcd(b, mod(a, b))
