open TechniCalcCalculator.Unit_Types

%%private(
  let prefixMmlSymbol = (prefix: prefix) =>
    switch prefix {
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
    }
)

%%private(
  let unitMmlSymbol = (unit: unitType) =>
    switch unit {
    | Second => "s"
    | Minute => "min"
    | Hour => "h"
    | Day => "d"
    | Week => "week"
    | Month => "month"
    | Year => "year"
    | Decade => "decade"
    | Century => "century"

    | Meter => "m"
    | Inch => "in"
    | Foot => "ft"
    | Yard => "yd"
    | Mile => "mi"
    | NauticalMile => "NM"
    | LightYear => "ly"
    | Parsec => "pc"
    | Angstrom => "&#x212B;"

    | Gram => "g"
    | Tonne => "T"
    | Ounce => "oz"
    | Pound => "lb"
    | Stone => "st"

    | Acre => "acre"
    | Hectare => "ha"

    | Liter => "l"
    | Gallon => "Gal"
    | USGallon => "US Gal"
    | Quart => "qt"
    | Cup => "cup"
    | USCup => "US cup"
    | Teaspoon => "tsp"
    | Tablespoon => "tbsp"
    | FluidOunce => "fl oz"

    | Knot => "kn"

    | Newton => "N"
    | PoundForce => "lbf"

    | Pascal => "Pa"
    | Atmosphere => "atm"
    | Bar => "bar"

    | Joule => "J"
    | Calorie => "cal"
    | ElectronVolt => "eV"
    | BTU => "Btu"
    | Therm => "thm"

    | Watt => "W"
    | Horsepower => "hp"
    | MetricHorsepower => "PS"

    | Bit => "b"
    | Byte => "B"

    | Kelvin => "K"
    | Celsius => "&#x00B0;C"
    | Fahrenheit => "&#x00B0;F"
    }
)

%%private(
  let prefixUnitMml = (prefix: prefix, unit: unitType) => {
    let unitMml = switch unit {
    | Angstrom => "A"
    | _ => unitMmlSymbol(unit)
    }
    `<mi mathvariant="normal">${prefixMmlSymbol(prefix)}${unitMml}</mi>`
  }
)

let unitPartMml = (. {prefix, unit, power}: unitPart) =>
  switch power {
  | 1 => prefixUnitMml(prefix, unit)
  | _ =>
    let powerMml = `<mn>${Belt.Int.toString(power)}</mn>`
    `<msup>${prefixUnitMml(prefix, unit)}${powerMml}</msup>`
  }

let toMml = (units: array<unitPart>) =>
  Belt.Array.mapU(units, unitPartMml)->StringUtil.joinWith("<mspace width=\"0.1em\" />")
