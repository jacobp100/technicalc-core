type prefix =
  | Unit
  | Femto
  | Pico
  | Nano
  | Micro
  | Milli
  | Centi
  | Deci
  | Deca
  | Hecto
  | Kilo
  | Mega
  | Giga
  | Tera
  | Peta
  | Exa
  | Kibi
  | Mebi
  | Gibi
  | Tebi
  | Pebi
  | Exbi

/*
 This would be much better as a polymorphic variant, but it meant any switch
 statement over values took over 1kb of JS to compile
 */
type unitType =
  /* Time */
  | Second
  | Minute
  | Hour
  | Day
  | Week
  | Month
  | Year
  | Decade
  | Century
  /* Length */
  | Meter
  | Inch
  | Foot
  | Yard
  | Mile
  | NauticalMile
  | ScandinavianMile
  | LightYear
  | Parsec
  | Angstrom
  /* Mass */
  | Gram
  | Tonne
  | Ounce
  | Pound
  | Stone
  /* Area */
  | Acre
  | Hectare
  /* Volume */
  | Liter
  | Gallon
  | USGallon
  | Quart
  | USQuart
  | Cup
  | USCup
  | Pint
  | USPint
  | Teaspoon
  | USTeaspoon
  | Tablespoon
  | USTablespoon
  | FluidOunce
  /* Speed */
  | Knot
  /* Force */
  | Newton
  | PoundForce
  /* Pressure */
  | Pascal
  | Atmosphere
  | Bar
  /* Energy */
  | Joule
  | Calorie
  | ElectronVolt
  | BTU
  | Therm
  /* Power */
  | Watt
  | Horsepower
  | MetricHorsepower
  /* Memory */
  | Bit
  | Byte
  /* Temperature */
  | Kelvin
  | Celsius
  | Fahrenheit

type t = {
  prefix: prefix,
  @as("type") type_: unitType,
  power: int,
}

let eq = (a, b) => a.prefix == b.prefix && a.type_ == b.type_ && a.power == b.power
