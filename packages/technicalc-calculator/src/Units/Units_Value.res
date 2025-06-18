open Units_Types

let prefixValue = (prefix: prefix) =>
  switch prefix {
  | Femto => Decimal.ofString("1e-15")->Real.ofDecimal
  | Pico => Decimal.ofString("1e-12")->Real.ofDecimal
  | Nano => Real.ofRational(1, 1000000000, Unit)
  | Micro => Real.ofRational(1, 1000000, Unit)
  | Milli => Real.ofRational(1, 1000, Unit)
  | Centi => Real.ofRational(1, 100, Unit)
  | Deci => Real.ofRational(1, 10, Unit)
  | Unit => Real.one
  | Deca => Real.ofInt(10)
  | Hecto => Real.ofInt(100)
  | Kilo => Real.ofInt(1000)
  | Mega => Real.ofInt(1000000)
  | Giga => Real.ofInt(1000000000)
  | Tera => Decimal.ofString("1e12")->Real.ofDecimal
  | Peta => Decimal.ofString("1e15")->Real.ofDecimal
  | Exa => Decimal.ofString("1e18")->Real.ofDecimal
  | Kibi => Real.ofInt(1024)
  | Mebi => Real.ofInt(1048576)
  | Gibi => Real.ofInt(1073741824)
  | Tebi => Decimal.ofString("1099511627776")->Real.ofDecimal
  | Pebi => Decimal.ofString("1125899906842624")->Real.ofDecimal
  | Exbi => Decimal.ofString("1152921504606846976")->Real.ofDecimal
  }

let linearValueExn = (name: name) =>
  switch name {
  | Second => Real.one
  | Minute => Real.ofInt(60)
  | Hour => Real.ofInt(3600)
  | Day => Real.ofInt(86400)
  | Week => Real.ofInt(604800)
  | Month => Real.ofInt(2628000)
  | Year => Real.ofInt(31536000)
  | Decade => Real.ofInt(315360000)
  | Century => Decimal.ofString("3155673600")->Real.ofDecimal

  | Meter => Real.one
  | Inch => Real.ofRational(254, 10000, Unit)
  | Foot => Real.ofRational(3048, 10000, Unit)
  | Yard => Real.ofRational(9144, 10000, Unit)
  | Mile => Real.ofInt(1609)
  | NauticalMile => Real.ofInt(1852)
  | ScandinavianMile => Real.ofInt(10000)
  | LightYear => Decimal.ofString("9460730472580800")->Real.ofDecimal
  | Parsec => Decimal.ofString("3.086e16")->Real.ofDecimal
  | Angstrom => Decimal.ofString("1e-10")->Real.ofDecimal
  | Furlong => Real.ofRational(201168, 1000, Unit)
  | Chain => Real.ofRational(201168, 10000, Unit)
  | Link => Real.ofRational(201168, 1000000, Unit)
  | Rod => Real.ofRational(50292, 10000, Unit)

  | Gram => Real.ofRational(1, 1000, Unit)
  | Tonne => Real.ofInt(1000)
  | Ounce => Real.ofRational(283495, 10000000, Unit)
  | Pound => Real.ofRational(453592, 1000000, Unit)
  | Stone => Real.ofRational(635029, 100000, Unit)

  | Acre => Real.ofInt(4047)
  | Hectare => Real.ofInt(10000)

  | Liter => Real.ofRational(1, 1000, Unit)
  | Gallon => Real.ofRational(454609, 100000000, Unit)
  | USGallon => Decimal.ofString("0.00378541200000013893")->Real.ofDecimal
  | Quart => Real.ofRational(113652, 100000000, Unit)
  | USQuart => Decimal.ofString("0.00094635300000003473239")->Real.ofDecimal
  | Cup => Real.ofRational(284131, 1000000000, Unit)
  | USCup => Real.ofRational(24, 100000, Unit)
  | Pint => Decimal.ofString("0.00056826128242566881568")->Real.ofDecimal
  | USPint => Decimal.ofString("0.0004731765000000173662")->Real.ofDecimal
  | Teaspoon => Decimal.ofString("5.919333333043e-6")->Real.ofDecimal
  | USTeaspoon => Decimal.ofString("4.92899841912e-6")->Real.ofDecimal
  | Tablespoon => Real.ofRational(17758, 1000000000, Unit)
  | USTablespoon => Real.ofRational(14787, 1000000000, Unit)
  | FluidOunce => Real.ofRational(28413, 1000000000, Unit)

  | Knot => Real.ofRational(514444, 1000000, Unit)

  | Newton => Real.one
  | PoundForce => Real.ofRational(4448222, 1000000, Unit)

  | Pascal => Real.one
  | Atmosphere => Real.ofInt(101325)
  | Bar => Real.ofInt(100000)

  | Joule => Real.one
  | Calorie => Real.ofRational(4184, 1000, Unit)
  | ElectronVolt => Decimal.ofString("1.602e-19")->Real.ofDecimal
  | BTU => Real.ofInt(1055)
  | Therm => Real.ofInt(1055000000)

  | Watt => Real.one
  | Horsepower => Real.ofRational(7457, 10, Unit)
  | MetricHorsepower => Real.ofRational(7355, 10, Unit)

  | Volt => Real.one

  | Ampere => Real.one

  | Ohm => Real.one

  | Coulomb => Real.one

  | Farad => Real.one

  | Weber => Real.one

  | Tesla => Real.one

  | Henry => Real.one

  | Siemens => Real.one

  | Mole => Real.one

  | Hertz => Real.one

  | Bit => Real.one
  | Byte => Real.ofInt(8)

  | Kelvin => Real.one
  | Celsius
  | Fahrenheit =>
    assert(false)
  }

let conversionFactorExn = (~powerMultiplier=1, {prefix, name, power}) => {
  let nextPower = power * powerMultiplier

  open Real
  (prefixValue(prefix) * linearValueExn(name)) ** ofInt(nextPower)
}
