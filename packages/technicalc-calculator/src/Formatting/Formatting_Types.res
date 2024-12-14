type mode =
  | @as(0) Ascii
  | @as(1) Unicode
  | @as(2) Tex
  | @as(3) MathML

type style =
  | @as(0) Natural({mixedFractions: bool})
  | @as(1) Decimal
  | @as(2) Engineering

type unitFormat = | @as(0) Exponential | @as(1) Operator

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

let defaultUnitFormat = Exponential
