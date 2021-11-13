let roundedRationalOfDecimal = (~denimonator as d, decimal: Decimal.t) => {
  open Decimal
  switch round(decimal * ofInt(d))->toFloat->FloatUtil.toInt {
  | Some(n) => Some(Real_Base.ofRational(n, d, Unit))
  | None => None
  }
}
