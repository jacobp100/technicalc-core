type mode =
  | Ascii
  | Unicode
  | Tex
  | MathML

type style =
  | Natural({mixedFractions: bool})
  | Decimal
  | Engineering

type format = {
  mode: mode,
  style: style,
  decimalSeparator: string,
  groupingSeparator: string,
  digitGrouping: bool,
  base: int,
  minDecimalPlaces: int,
  maxDecimalPlaces: int,
  decimalMinMagnitude: int,
  decimalMaxMagnitude: int,
}

let defaultFormat = {
  mode: Ascii,
  style: Decimal,
  decimalSeparator: ".",
  groupingSeparator: ",",
  digitGrouping: true,
  base: 10,
  minDecimalPlaces: 0,
  maxDecimalPlaces: 12,
  decimalMinMagnitude: -3,
  decimalMaxMagnitude: 8,
}
