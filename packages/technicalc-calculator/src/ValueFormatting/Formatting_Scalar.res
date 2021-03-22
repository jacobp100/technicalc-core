open Formatting_Types
open Formatting_Util

%%private(
  let formatNumber = (x, format) =>
    switch format {
    | Some({mode: MathML}) => `<mn>${x}</mn>`
    | _ => x
    }
)

%%private(
  let formatExponential = ((base, exponent), format) =>
    switch format {
    | Some({mode: String})
    | None =>
      `${base}e${exponent}`
    | Some({mode: Unicode}) =>
      base ++ Formatting_Unicode.magnitude ++ Formatting_Unicode.formatSuperscriptNumbers(exponent)
    | Some({mode: Tex}) => `${base}*10*{${exponent}}`
    | Some({mode: MathML}) =>
      let base = formatNumber(base, format)
      let exponent = formatNumber(exponent, format)
      `${base}<mo>&#xD7;</mo><msup><mn>10</mn>${exponent}</msup>`
    }
)

%%private(
  let formatRow = (x, format) =>
    switch format {
    | Some({mode: MathML}) => `<mrow>${x}</mrow>`
    | _ => x
    }
)

%%private(
  let formatConstantMultiple = (~formatAsRow, n, c, format): string => {
    let {locale, base, digitGrouping} = switch format {
    | Some(format) => format
    | None => defaultFormat
    }

    switch (
      Decimal.ofInt(n)->Formatting_Number.formatInteger(~locale, ~base, ~digitGrouping, _),
      Formatting_Constant.toString(~format, c),
    ) {
    | ("1", "") => formatNumber("1", format)
    | ("1", constant) => constant
    | (numerator, constant) =>
      let out = formatNumber(numerator, format) ++ constant
      formatAsRow ? formatRow(out, format) : out
    }
  }
)

%%private(
  let formatReal = (re, format): string => {
    let {locale, base, digitGrouping} = switch format {
    | Some(format) => format
    | None => defaultFormat
    }

    switch (re, format) {
    | (Real.Rational(n, 1, c), Some({style: Natural(_)})) =>
      let minus = n < 0 ? formatOperator("-", format) : ""

      let n = IntUtil.abs(n)
      let constantMultiple = formatConstantMultiple(~formatAsRow=false, n, c, format)
      minus ++ constantMultiple
    | (Rational(n, d, c), Some({mode, style: Natural({mixedFractions})})) =>
      let minus = n < 0 ? formatOperator("-", format) : ""
      let n = IntUtil.abs(n)

      let (before, n) = if mixedFractions && c == Unit {
        let integer = n / d
        let remainder = IntUtil.safeMod(n, d)
        let before =
          integer == 0
            ? ""
            : Decimal.ofInt(integer)
              ->Formatting_Number.formatInteger(~locale, ~base, ~digitGrouping, _)
              ->formatNumber(format)
        (before, remainder)
      } else {
        ("", n)
      }

      let top = formatConstantMultiple(~formatAsRow=true, n, c, format)

      let bottom =
        Decimal.ofInt(d)
        ->Formatting_Number.formatInteger(~locale, ~base, ~digitGrouping, _)
        ->formatNumber(format)

      switch mode {
      | String
      | Unicode =>
        let divide = mode == String ? "/" : Formatting_Unicode.divide
        let out = top ++ divide ++ bottom
        let out = before != "" ? `${before}+${out}` : out
        let out = before != "" && minus != "" ? `${minus}(${out})` : minus ++ out
        out
      | Tex => `${minus}${before}${"\\"}frac{${top}}{${bottom}}`
      | MathML => `${minus}${before}<mfrac>${top}${bottom}</mfrac>`
      }
    | (_, Some({style: Natural(_) | Decimal} as format')) =>
      let {decimalMinMagnitude, decimalMaxMagnitude, precision} = format'
      let f = Real.toDecimal(re)
      let magnitude = DecimalUtil.magnitude(f)
      let insideMagnitudeThreshold = {
        open Decimal
        magnitude >= ofInt(decimalMinMagnitude) && magnitude <= ofInt(decimalMaxMagnitude)
      }

      if insideMagnitudeThreshold {
        Formatting_Number.formatDecimal(
          ~locale,
          ~base,
          ~digitGrouping,
          ~maxDecimalPlaces=precision,
          f,
        )->formatNumber(format)
      } else {
        Formatting_Number.formatExponential(
          ~locale,
          ~base,
          ~exponent=magnitude,
          ~maxDecimalPlaces=precision,
          f,
        )->formatExponential(format)
      }
    | (_, Some({style: Engineering, precision})) =>
      let f = Real.toDecimal(re)
      let magnitude = DecimalUtil.magnitude(f)

      let exponent = {
        open Decimal
        floor(magnitude / ofInt(3)) * ofInt(3)
      }

      Formatting_Number.formatExponential(
        ~locale,
        ~base,
        ~exponent,
        ~minDecimalPlaces=precision,
        ~maxDecimalPlaces=precision,
        f,
      )->formatExponential(format)
    | (_, None) => Real.toDecimal(re)->Decimal.toString
    }
  }
)

%%private(
  let formatImag = (re: Real.t, format): string => {
    let i = formatVariable("i", format)
    let compact = switch format {
    | Some({style: Natural(_) | Decimal})
    | None => true
    | _ => false
    }
    switch (compact, re) {
    | (true, Rational(1, 1, Unit)) => i
    | (true, Rational(-1, 1, Unit)) => formatOperator("-", format) ++ i
    | _ => formatReal(re, format) ++ i
    }
  }
)

let toString = (~format=?, a: Scalar.t): string =>
  switch a {
  | #Z => formatNumber("0", format)
  | #R(re) => formatReal(re, format)
  | #I(im) => formatImag(im, format)
  | #C(re, im) =>
    formatReal(re, format) ++
    formatOperator(Decimal.lt(Real.toDecimal(im), Decimal.zero) ? "-" : "+", format) ++
    formatImag(Real.abs(im), format)
  }
