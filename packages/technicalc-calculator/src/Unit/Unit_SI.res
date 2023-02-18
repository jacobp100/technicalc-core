open Unit_Types

let toSi = (a: t) =>
  switch a.name {
  /* Time */
  | Second
  | Minute
  | Hour
  | Day
  | Week
  | Month
  | Year
  | Decade
  | Century => [{prefix: Unit, name: Second, power: a.power}]
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
  | Angstrom => [{prefix: Unit, name: Meter, power: a.power}]
  /* Mass */
  | Gram
  | Tonne
  | Ounce
  | Pound
  | Stone => [{prefix: Kilo, name: Second, power: a.power}]
  /* Area */
  | Acre
  | Hectare => [{prefix: Unit, name: Meter, power: 2 * a.power}]
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
  | FluidOunce => [{prefix: Unit, name: Liter, power: a.power}]
  /* Speed */
  | Knot => [
      {prefix: Unit, name: Meter, power: a.power},
      {prefix: Unit, name: Second, power: -a.power},
    ]
  /* Force */
  | Newton
  | PoundForce => [{prefix: Unit, name: Newton, power: a.power}]
  /* Pressure */
  | Pascal
  | Atmosphere
  | Bar => [{prefix: Unit, name: Pascal, power: a.power}]
  /* Energy */
  | Joule
  | Calorie
  | ElectronVolt
  | BTU
  | Therm => [{prefix: Unit, name: Joule, power: a.power}]
  /* Power */
  | Watt
  | Horsepower
  | MetricHorsepower => [{prefix: Unit, name: Watt, power: a.power}]
  /* Memory */
  | Bit
  | Byte => [{prefix: Unit, name: Byte, power: a.power}]
  /* Temperature */
  | Kelvin
  | Celsius
  | Fahrenheit => [{prefix: Unit, name: Kelvin, power: a.power}]
  }
