open TechniCalcCalculator.Unit_Types;

let%private prefixMmlSymbol = (prefix: prefix) =>
  switch (prefix) {
  | Femto => "f"
  | Pico => "p"
  | Nano => "n"
  | Micro => "&#x3BC;"
  | Milli => "m"
  | Centi => "c"
  | Deci => "d"
  | Unit => ""
  | Deca => "da"
  | Hecto => "h"
  | Kilo => "k"
  | Mega => "M"
  | Giga => "G"
  | Tera => "T"
  | Peta => "P"
  | Kibi => "Ki"
  | Mebi => "Mi"
  | Gibi => "Gi"
  | Tebi => "Ti"
  | Pebi => "Pi"
  };

let%private unitMmlSymbol = (unit: unitType) =>
  switch (unit) {
  /* Time */
  | Second => "s"
  | Minute => "min"
  | Hour => "h"
  | Day => "d"
  | Week => "week"
  | Month => "month"
  | Year => "year"
  | Decade => "decade"
  | Century => "century"
  /* Length */
  | Meter => "m"
  | Inch => "in"
  | Foot => "ft"
  | Yard => "yd"
  | Mile => "mi"
  | NauticalMile => "NM"
  | LightYear => "ly"
  | Parsec => "pc"
  | Angstrom => "&#x212B;"
  /* Mass */
  | Gram => "g"
  | Tonne => "T"
  | Ounce => "oz"
  | Pound => "lb"
  | Stone => "st"
  /* Area */
  | Acre => "acre"
  | Hectare => "ha"
  /* Volume */
  | Liter => "l"
  | Gallon => "Gal"
  | USGallon => "US Gal"
  | Quart => "qt"
  | Cup => "cup"
  | USCup => "US cup"
  | Teaspoon => "tsp"
  | Tablespoon => "tbsp"
  | FluidOunce => "fl oz"
  /* Speed */
  | Knot => "kn"
  /* Force */
  | Newton => "N"
  | PoundForce => "lbf"
  /* Pressure */
  | Pascal => "Pa"
  | Atmosphere => "atm"
  | Bar => "bar"
  /* Energy */
  | Joule => "J"
  | Calorie => "cal"
  | ElectronVolt => "eV"
  | BTU => "Btu"
  | Therm => "thm"
  /* Power */
  | Watt => "W"
  | Horsepower => "hp"
  | MetricHorsepower => "PS"
  /* Memory */
  | Bit => "b"
  | Byte => "B"
  /* Temperature */
  | Kelvin => "K"
  | Celsius => "&#x00B0;C"
  | Fahrenheit => "&#x00B0;F"
  };

let%private prefixUnitMml = (prefix: prefix, unit: unitType) =>
  switch (unit) {
  /* Work around :( */
  | Angstrom =>
    "<mi mathvariant=\"normal\">" ++ prefixMmlSymbol(prefix) ++ "A</mi>"
  | _ =>
    "<mi mathvariant=\"normal\">"
    ++ prefixMmlSymbol(prefix)
    ++ unitMmlSymbol(unit)
    ++ "</mi>"
  };

let%private unitPowerMml =
  (. {prefix, unit, power}: unitPart) =>
    switch (power) {
    | 1 => prefixUnitMml(prefix, unit)
    | _ =>
      let powerMml = "<mn>" ++ Belt.Int.toString(power) ++ "</mn>";
      "<msup>" ++ prefixUnitMml(prefix, unit) ++ powerMml ++ "</msup>";
    };

let toMml = (units: array(unitPart)) =>
  Belt.Array.mapU(units, unitPowerMml)
  ->StringUtil.joinWith("<mspace width=\"0.1em\" />");
