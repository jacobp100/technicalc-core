open Formatting_Types
open Formatting_Util

%%private(
  let formatNumber = (~format, x) =>
    switch format.mode {
    | MathML => `<mn>${x}</mn>`
    | _ => x
    }
)

%%private(
  let formatExponential = (~format, (base, exponent)) =>
    switch format.mode {
    | Ascii => `${base}e${exponent}`
    | Unicode =>
      base ++ Formatting_Unicode.magnitude ++ Formatting_Unicode.formatSuperscriptNumbers(exponent)
    | Tex => `${base}*10*{${exponent}}`
    | MathML =>
      let base = formatNumber(~format, base)
      let exponent = formatNumber(~format, exponent)
      `${base}<mo>&#xD7;</mo><msup><mn>10</mn>${exponent}</msup>`
    }
)

%%private(
  let formatRow = (~format, x) =>
    switch format.mode {
    | MathML => `<mrow>${x}</mrow>`
    | _ => x
    }
)

%%private(
  let formatConstantMultiple = (~format, ~formatAsRow, n, c): string => {
    let {decimalSeparator, groupingSeparator, base, digitGrouping} = format

    switch (
      Decimal.ofInt(n)->Formatting_Number.formatInteger(
        ~decimalSeparator,
        ~groupingSeparator,
        ~base,
        ~digitGrouping,
        _,
      ),
      Formatting_Constant.toString(~format, c),
    ) {
    | ("1", "") => formatNumber(~format, "1")
    | ("1", constant) => constant
    | (numerator, constant) =>
      let out = formatNumber(~format, numerator) ++ constant
      formatAsRow ? formatRow(~format, out) : out
    }
  }
)

let toString = (~format, re): string => {
  let {
    mode,
    decimalSeparator,
    groupingSeparator,
    base,
    digitGrouping,
    decimalMinMagnitude,
    decimalMaxMagnitude,
    precision,
  } = format

  switch (re, format.style) {
  | (Real.Rational(n, 1, c), Natural(_)) =>
    let minus = n < 0 ? formatOperator(~format, "-") : ""

    let n = IntUtil.abs(n)
    let constantMultiple = formatConstantMultiple(~format, ~formatAsRow=false, n, c)
    minus ++ constantMultiple
  | (Rational(n, d, c), Natural({mixedFractions})) =>
    let minus = n < 0 ? formatOperator(~format, "-") : ""
    let n = IntUtil.abs(n)

    let (before, n) = if mixedFractions && c == Unit {
      let integer = n / d
      let remainder = IntUtil.safeMod(n, d)->Belt.Option.getWithDefault(0)
      let before =
        integer == 0
          ? ""
          : Decimal.ofInt(integer)
            ->Formatting_Number.formatInteger(
              ~decimalSeparator,
              ~groupingSeparator,
              ~base,
              ~digitGrouping,
              _,
            )
            ->formatNumber(~format)
      (before, remainder)
    } else {
      ("", n)
    }

    let top = formatConstantMultiple(~format, ~formatAsRow=true, n, c)

    let bottom =
      Decimal.ofInt(d)
      ->Formatting_Number.formatInteger(
        ~decimalSeparator,
        ~groupingSeparator,
        ~base,
        ~digitGrouping,
        _,
      )
      ->formatNumber(~format)

    switch mode {
    | Ascii
    | Unicode =>
      let divide = mode == Ascii ? "/" : Formatting_Unicode.divide
      let out = top ++ divide ++ bottom
      let out = before != "" ? `${before}+${out}` : out
      let out = before != "" && minus != "" ? `${minus}(${out})` : minus ++ out
      out
    | Tex => `${minus}${before}\\frac{${top}}{${bottom}}`
    | MathML => `${minus}${before}<mfrac>${top}${bottom}</mfrac>`
    }
  | (_, Natural(_) | Decimal) =>
    let f = Real.toDecimal(re)
    let magnitude = DecimalUtil.magnitude(f)
    let insideMagnitudeThreshold = {
      open Decimal
      magnitude >= ofInt(decimalMinMagnitude) && magnitude <= ofInt(decimalMaxMagnitude)
    }

    if insideMagnitudeThreshold {
      Formatting_Number.formatDecimal(
        ~decimalSeparator,
        ~groupingSeparator,
        ~base,
        ~digitGrouping,
        ~maxDecimalPlaces=precision,
        f,
      )->formatNumber(~format)
    } else {
      Formatting_Number.formatExponential(
        ~decimalSeparator,
        ~groupingSeparator,
        ~base,
        ~exponent=magnitude,
        ~maxDecimalPlaces=precision,
        f,
      )->formatExponential(~format)
    }
  | (_, Engineering) =>
    let f = Real.toDecimal(re)
    let magnitude = DecimalUtil.magnitude(f)

    let exponent = {
      open Decimal
      floor(magnitude / ofInt(3)) * ofInt(3)
    }

    Formatting_Number.formatExponential(
      ~decimalSeparator,
      ~groupingSeparator,
      ~base,
      ~exponent,
      ~minDecimalPlaces=precision,
      ~maxDecimalPlaces=precision,
      f,
    )->formatExponential(~format)
  }
}
