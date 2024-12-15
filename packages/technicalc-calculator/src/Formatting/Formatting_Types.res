type mode =
  | @as(0) Ascii
  | @as(1) Unicode
  | @as(2) Tex
  | @as(3) MathML

type fractionMode =
  | @as(0) Never
  | @as(1) Improper
  | @as(2) Mixed

type exponentMode =
  | @as(0) Scientific
  | @as(1) Engineering

type unitFormat = | @as(0) Exponential | @as(1) Operator

type format = {
  mode: mode,
  constants: bool,
  fractions: fractionMode,
  exponents: exponentMode,
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
  constants: true,
  fractions: Improper,
  exponents: Scientific,
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
