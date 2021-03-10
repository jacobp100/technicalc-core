let maxInt = Js.Int.max

let abs = x => lor(Js.Math.abs_int(x), 0)

let safeMod = (a, b) => mod(mod(a, b) + b, b)
