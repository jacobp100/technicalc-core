open Formatting_Types
open Formatting_Util

%%private(
  let formatImag = (~format, re: Real.t): string => {
    let {base} = format
    let i = formatVariable(~format, "i")
    let magnitude = Real.toDecimal(re)->DecimalUtil.magnitude(~base)
    switch (Formatting_Real.insideMagnitudeThreshold(~format, ~magnitude), re) {
    | (true, Rational(1, 1, Unit)) => i
    | (true, Rational(-1, 1, Unit)) => formatOperator(~format, "-") ++ i
    | _ => Formatting_Real.toString(~format, re) ++ i
    }
  }
)

let toString = (~format, a: Scalar.t): string =>
  switch a {
  | #Zero => Formatting_Real.toString(~format, Real.zero)
  | #Real(re) => Formatting_Real.toString(~format, re)
  | #Imag(im) => formatImag(~format, im)
  | #Cmpx(re, im) =>
    Formatting_Real.toString(~format, re) ++
    formatOperator(~format, Decimal.lt(Real.toDecimal(im), Decimal.zero) ? "-" : "+") ++
    formatImag(~format, Real.abs(im))
  | #NaNN => formatVariable(~format, "NaN")
  }
