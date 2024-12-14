open Units_Types

let prefixes = Belt.Array.makeBy(21 + 1, i => {
  let prefix = Obj.magic(i)

  switch prefix {
  | Femto => "f"
  | Pico => "p"
  | Nano => "n"
  | Micro => "u"
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
})

let names = Belt.Array.makeBy(73 + 1, i => {
  let unit = Obj.magic(i)

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
  | Furlong => "furlong"
  | Chain => "chain"
  | Link => "link"
  | Rod => "rod"

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

  | Volt => "V"

  | Ampere => "A"

  | Ohm => "Ohm"

  | Coulomb => "C"

  | Farad => "F"

  | Weber => "Wb"

  | Tesla => "T"

  | Henry => "H"

  | Siemens => "S"

  | Mole => "mol"

  | Hertz => "Hz"

  | Bit => "b"
  | Byte => "B"

  | Kelvin => "K"
  | Celsius => "deg C"
  | Fahrenheit => "deg F"
  }
})
