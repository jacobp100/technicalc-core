type mode =
  | String
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
  precision: int,
  decimalMinMagnitude: int,
  decimalMaxMagnitude: int,
}

let defaultFormat = {
  mode: String,
  style: Natural({mixedFractions: false}),
  decimalSeparator: ".",
  groupingSeparator: ",",
  digitGrouping: true,
  base: 10,
  precision: 12,
  decimalMinMagnitude: -3,
  decimalMaxMagnitude: 8,
}
