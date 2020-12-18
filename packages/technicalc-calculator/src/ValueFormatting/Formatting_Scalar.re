open Formatting_Types;
open Formatting_Util;

let%private formatNumber = (x, format) =>
  switch (format) {
  | Some({mode: MathML}) => "<mn>" ++ x ++ "</mn>"
  | _ => x
  };

let%private formatExponential = ((base, exponent), format) =>
  switch (format) {
  | Some({mode: String})
  | None => base ++ "e" ++ exponent
  | Some({mode: Unicode}) =>
    base
    ++ Formatting_Unicode.magnitude
    ++ Formatting_Unicode.formatSuperscriptNumbers(exponent)
  | Some({mode: Tex}) => base ++ "*10^{" ++ exponent ++ "}"
  | Some({mode: MathML}) =>
    formatNumber(base, format)
    ++ "<mo>&#xD7;</mo><msup><mn>10</mn>"
    ++ formatNumber(exponent, format)
    ++ "</msup>"
  };

let%private formatTuple = (re, format): string => {
  let (base, digitGrouping) =
    switch (format) {
    | Some(format) => (Some(format.base), Some(format.digitGrouping))
    | None => (None, None)
    };

  switch (re, format) {
  | (Real.Rational(n, d, c), Some({style: Natural}) | None) =>
    let minus = n < 0 ? formatOperator("-", format) : "";

    let (top, needsWrap) =
      switch (
        Formatting_Number.formatInteger(
          ~base?,
          ~digitGrouping?,
          IntUtil.abs(n)->Decimal.ofInt,
        ),
        Formatting_Constant.toString(~format, c),
      ) {
      | ("1", "") => (formatNumber("1", format), false)
      | ("1", constant) => (constant, false)
      | (numerator, constant) => (
          formatNumber(numerator, format) ++ constant,
          true,
        )
      };

    switch (
      format,
      Formatting_Number.formatInteger(
        ~base?,
        ~digitGrouping?,
        Decimal.ofInt(d),
      ),
    ) {
    | (_, "1") => minus ++ top
    | (Some({mode: String}) | None, bottom) => minus ++ top ++ "/" ++ bottom
    | (Some({mode: Unicode}), bottom) =>
      minus ++ top ++ Formatting_Unicode.divide ++ bottom
    | (Some({mode: Tex}), bottom) =>
      minus ++ "\\frac{" ++ top ++ "}{" ++ bottom ++ "}"
    | (Some({mode: MathML}), denominator) =>
      let top = needsWrap ? "<mrow>" ++ top ++ "</mrow>" : top;
      let bottom = formatNumber(denominator, format);
      minus ++ "<mfrac>" ++ top ++ bottom ++ "</mfrac>";
    };
  | (
      _,
      Some({
        style: Natural | Decimal,
        decimalMinMagnitude,
        decimalMaxMagnitude,
        precision,
      }),
    ) =>
    let f = Real.toDecimal(re);
    let valueMagnitude = DecimalUtil.magnitude(f);
    let insideMagnitudeThreshold =
      valueMagnitude >= decimalMinMagnitude
      && valueMagnitude <= decimalMaxMagnitude;

    if (insideMagnitudeThreshold) {
      Formatting_Number.formatDecimal(
        ~base?,
        ~digitGrouping?,
        ~maxDecimalPlaces=precision,
        f,
      )
      ->formatNumber(format);
    } else {
      Formatting_Number.formatExponential(
        ~base?,
        ~exponent=DecimalUtil.magnitude(f),
        ~maxDecimalPlaces=precision,
        f,
      )
      ->formatExponential(format);
    };
  | (_, Some({style: Engineering, precision})) =>
    let f = Real.toDecimal(re);
    let magnitude = DecimalUtil.magnitude(f);
    /* Round to multiple of 3 */
    let exponent =
      if (magnitude >= 0) {
        magnitude / 3 * 3;
      } else {
        - ((- magnitude + 2) / 3 * 3);
      };
    Formatting_Number.formatExponential(
      ~base?,
      ~exponent,
      ~minDecimalPlaces=precision,
      ~maxDecimalPlaces=precision,
      f,
    )
    ->formatExponential(format);
  | (Decimal(f), None) => Decimal.toString(f)
  };
};

let%private formatImagTuple = (re: Real.t, format): string => {
  let i = formatVariable("i", format);
  let compact =
    switch (format) {
    | Some({style: Natural | Decimal})
    | None => true
    | _ => false
    };
  switch (compact, re) {
  | (true, Rational(1, 1, Unit)) => i
  | (true, Rational((-1), 1, Unit)) => formatOperator("-", format) ++ i
  | _ => formatTuple(re, format) ++ i
  };
};

let toString = (~format=?, a: Scalar.t): string =>
  switch (a) {
  | `Z => formatNumber("0", format)
  | `R(re) => formatTuple(re, format)
  | `I(im) => formatImagTuple(im, format)
  | `C(re, im) =>
    formatTuple(re, format)
    ++ formatOperator(
         Decimal.(Real.toDecimal(im) < zero) ? "-" : "+",
         format,
       )
    ++ formatImagTuple(Real.abs(im), format)
  };
