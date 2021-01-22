type mode =
  | String
  | Unicode
  | Tex
  | MathML;

type style =
  | Natural({mixedFractions: bool})
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

let defaultFormat = {
  mode: String,
  style: Natural({mixedFractions: false}),
  base: 10,
  precision: 12,
  digitGrouping: true,
  decimalMinMagnitude: (-3),
  decimalMaxMagnitude: 8,
};
