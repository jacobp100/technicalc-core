let ratDecimal = (n: int, d: int, c: Real_Constant.t) => {
  let base = {
    open Decimal
    ofInt(n) / ofInt(d)
  }

  c == Unit ? base : Decimal.mul(base, Real_Constant.toDecimal(c))
}
