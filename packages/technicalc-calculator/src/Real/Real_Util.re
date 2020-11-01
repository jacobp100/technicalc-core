let ratDecimal = (n: int, d: int, c: Real_Constant.t) => {
  let base = Decimal.(ofInt(n) / ofInt(d));
  c == Unit ? base : Decimal.(base * Real_Constant.toDecimal(c));
};

let rec gcd = (a, b) => b == 0 ? a : gcd(b, a mod b);
