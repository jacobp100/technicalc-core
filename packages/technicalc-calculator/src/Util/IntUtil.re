let maxInt = Js.Int.max;

let abs = x => Js.Math.abs_int(x) lor 0;

let safeMod = (a, b) => (a mod b + b) mod b;
