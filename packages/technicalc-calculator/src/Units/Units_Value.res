open Units_Types

let prefixValue = (prefix: prefix) =>
  Decimal.ofString(
    switch prefix {
    | Femto => "1e-15"
    | Pico => "1e-12"
    | Nano => "1e-9"
    | Micro => "1e-6"
    | Milli => "1e-3"
    | Centi => "1e-2"
    | Deci => "1e-1"
    | Unit => "1e0"
    | Deca => "1e1"
    | Hecto => "1e2"
    | Kilo => "1e3"
    | Mega => "1e6"
    | Giga => "1e9"
    | Tera => "1e12"
    | Peta => "1e15"
    | Exa => "1e18"
    | Kibi => "1024"
    | Mebi => "1048576"
    | Gibi => "1073741824"
    | Tebi => "1099511627776"
    | Pebi => "1125899906842624"
    | Exbi => "1152921504606846976"
    },
  )

let linearValueExn = (name: name) =>
  Decimal.ofString(
    switch name {
    | Second => "1"
    | Minute => "60"
    | Hour => "3600"
    | Day => "86400"
    | Week => "604800"
    | Month => "2628000"
    | Year => "31536000"
    | Decade => "315360000"
    | Century => "3155673600"

    | Meter => "1"
    | Inch => "0.0254"
    | Foot => "0.3048"
    | Yard => "0.9144"
    | Mile => "1609"
    | NauticalMile => "1852"
    | ScandinavianMile => "10000"
    | LightYear => "9460730472580800"
    | Parsec => "3.086e16"
    | Angstrom => "1e-10"

    | Gram => "1e-3"
    | Tonne => "1e3"
    | Ounce => "0.0283495"
    | Pound => "0.453592"
    | Stone => "6.35029"

    | Acre => "4047"
    | Hectare => "1e4"

    | Liter => "1e-3"
    | Gallon => "0.00454609"
    | USGallon => "0.00378541200000013893"
    | Quart => "0.00113652"
    | USQuart => "0.00094635300000003473239"
    | Cup => "0.000284131"
    | USCup => "0.00024"
    | Pint => "0.00056826128242566881568"
    | USPint => "0.0004731765000000173662"
    | Teaspoon => "5.919333333043e-6"
    | USTeaspoon => "4.92899841912e-6"
    | Tablespoon => "1.7758e-5"
    | USTablespoon => "1.4787e-5"
    | FluidOunce => "2.8413e-5"

    | Knot => "0.514444"

    | Newton => "1"
    | PoundForce => "4.448222"

    | Pascal => "1"
    | Atmosphere => "101325"
    | Bar => "100000"

    | Joule => "1"
    | Calorie => "4.184"
    | ElectronVolt => "1.602e-19"
    | BTU => "1055"
    | Therm => "1055000000"

    | Watt => "1"
    | Horsepower => "745.7"
    | MetricHorsepower => "735.5"

    | Volt => "1"

    | Ampere => "1"

    | Ohm => "1"

    | Coulomb => "1"

    | Farad => "1"

    | Weber => "1"

    | Tesla => "1"

    | Henry => "1"

    | Siemens => "1"

    | Mole => "1"

    | Hertz => "1"

    | Bit => "1"
    | Byte => "8"

    | Kelvin => "1"
    | Celsius
    | Fahrenheit =>
      assert false
    },
  )

let conversionFactorExn = (~powerMultiplier=1, {prefix, name, power}) => {
  let nextPower = power * powerMultiplier

  open Decimal
  (prefixValue(prefix) * linearValueExn(name)) ** ofInt(nextPower)
}
