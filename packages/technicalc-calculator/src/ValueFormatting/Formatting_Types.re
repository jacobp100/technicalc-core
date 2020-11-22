type mode =
  | String
  | Unicode
  | Tex
  | MathML;

type style =
  | Natural
  | Decimal
  | Engineering;

type format = {
  mode,
  style,
  base: int,
  precision: int,
  digitGrouping: bool,
  decimalMinMagnitude: int,
  decimalMaxMagnitude: int,
};

let default = {
  mode: String,
  style: Natural,
  base: 10,
  precision: 12,
  digitGrouping: true,
  decimalMinMagnitude: (-3),
  decimalMaxMagnitude: 8,
};
