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
  let formatExponential = (~format, (decimal, base, exponent)) =>
    switch format.mode {
    | Ascii => `${decimal}e${exponent}`
    | Unicode =>
      decimal ++
      Formatting_Unicode.multiply ++
      base ++
      Formatting_Unicode.formatSuperscriptNumbers(exponent)
    | Tex => `${decimal}*${base}^{${exponent}}`
    | MathML =>
      let decimal = formatNumber(~format, decimal)
      let exponent = formatNumber(~format, exponent)
      `${decimal}<mo>&#xD7;</mo><msup><mn>${base}</mn>${exponent}</msup>`
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
      Decimal.ofInt(n)->(
        Formatting_Number.formatInteger(
          ~decimalSeparator,
          ~groupingSeparator,
          ~base,
          ~digitGrouping,
          _,
        )
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

%%private(
  let formatInteger = (~format, i, c): string => {
    let minus = i < 0 ? formatOperator(~format, "-") : ""
    let i = IntUtil.abs(i)
    let constantMultiple = formatConstantMultiple(~format, ~formatAsRow=false, i, c)
    minus ++ constantMultiple
  }
)

%%private(
  let formatFraction = (~format, n: int, d: int, c: Real_Constant.t): string => {
    let {mode, fractions, decimalSeparator, groupingSeparator, base, digitGrouping} = format

    let minus = n < 0 ? formatOperator(~format, "-") : ""
    let n = IntUtil.abs(n)

    let (before, n) = if fractions == Mixed && c == Unit {
      let integer = n / d
      let remainder = IntUtil.safeMod(n, d)->Belt.Option.getWithDefault(0)
      let before =
        integer == 0
          ? ""
          : Decimal.ofInt(integer)
            ->(Formatting_Number.formatInteger(
              ~decimalSeparator,
              ~groupingSeparator,
              ~base,
              ~digitGrouping,
              _,
            ))
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
  }
)

%%private(
  let formatRational = (~format, n: int, d: int, c: Real_Constant.t) => {
    if c != Unit && !format.constants {
      None
    } else if d == 1 {
      formatInteger(~format, n, c)->Some
    } else if format.fractions != Never {
      formatFraction(~format, n, d, c)->Some
    } else {
      None
    }
  }
)

let insideMagnitudeThreshold = (~format, ~magnitude) => {
  let {decimalMinMagnitude, decimalMaxMagnitude} = format
  decimalMinMagnitude != decimalMaxMagnitude && {
      open Decimal
      magnitude >= ofInt(decimalMinMagnitude) && magnitude <= ofInt(decimalMaxMagnitude)
    }
}

let toString = (~format, re): string => {
  let {
    exponents,
    decimalSeparator,
    groupingSeparator,
    base,
    digitGrouping,
    minDecimalPlaces,
    maxDecimalPlaces,
  } = format

  let f = Real.toDecimal(re)
  let magnitude = DecimalUtil.magnitude(~base, f)

  if insideMagnitudeThreshold(~format, ~magnitude) {
    let rational = switch re {
    | Real.Rational(n, d, c) => formatRational(~format, n, d, c)
    | _ => None
    }
    switch rational {
    | Some(rational) => rational
    | _ =>
      Formatting_Number.formatDecimal(
        ~decimalSeparator,
        ~groupingSeparator,
        ~base,
        ~digitGrouping,
        ~minDecimalPlaces,
        ~maxDecimalPlaces,
        f,
      )->formatNumber(~format)
    }
  } else {
    let rounding = base == 10 ? 3 : 4
    let exponent = switch exponents {
    | Engineering =>
      open Decimal
      floor(magnitude / ofInt(rounding)) * ofInt(rounding)
    | Scientific => magnitude
    }
    Formatting_Number.formatExponential(
      ~decimalSeparator,
      ~groupingSeparator,
      ~base,
      ~exponent,
      ~minDecimalPlaces,
      ~maxDecimalPlaces,
      f,
    )->formatExponential(~format)
  }
}
