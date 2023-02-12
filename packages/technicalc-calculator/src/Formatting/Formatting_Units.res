open Formatting_Types
open Units_Types

let formatPrefix = (~mode, prefix: prefix) =>
  switch prefix {
  | Femto => "f"
  | Pico => "p"
  | Nano => "n"
  | Micro =>
    switch mode {
    | MathML => "&#x3BC;"
    | Tex => `\\micro`
    | Ascii => "u"
    | Unicode => Formatting_Unicode.mu
    }
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
  | Exa => "E"
  | Kibi => "Ki"
  | Mebi => "Mi"
  | Gibi => "Gi"
  | Tebi => "Ti"
  | Pebi => "Pi"
  | Exbi => "Ei"
  }

%%private(
  let deg = (~mode) =>
    switch mode {
    | MathML => "&#x00B0;"
    | Tex => `{}^{\\circ}`
    | Ascii => "deg "
    | Unicode => Formatting_Unicode.degree
    }
)

let formatName = (~mode, unit: name) =>
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
  | ScandinavianMile => "mil"
  | LightYear => "ly"
  | Parsec => "pc"
  | Angstrom => "A" // "&#x212B;"

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
  | USQuart => "US qt"
  | Cup => "cup"
  | USCup => "US cup"
  | Pint => "pt"
  | USPint => "US pt"
  | Teaspoon => "tsp"
  | USTeaspoon => "US tsp"
  | Tablespoon => "tbsp"
  | USTablespoon => "US tbsp"
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
  | Celsius => deg(~mode) ++ "C"
  | Fahrenheit => deg(~mode) ++ "F"
  }

%%private(
  let formatUnit = (~mode, ~prefix: prefix, ~name: name) => {
    let body = formatPrefix(~mode, prefix) ++ formatName(~mode, name)
    switch mode {
    | MathML => `<mi mathvariant="normal">${body}</mi>`
    | _ => body
    }
  }
)

let toString = (~mode, {prefix, name, power}: t) => {
  let formattedUnit = formatUnit(~mode, ~prefix, ~name)

  switch power {
  | 1 => formattedUnit
  | _ =>
    let power = Belt.Int.toString(power)
    switch mode {
    | MathML => `<msup>${formattedUnit}<mn>${power}</mn></msup>`
    | Tex => `${formattedUnit}^{${power}}`
    | Ascii => `${formattedUnit}^${power}`
    | Unicode => `${formattedUnit}^${Formatting_Unicode.formatSuperscriptNumbers(power)}`
    }
  }
}
