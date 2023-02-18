open Units_Types

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
  /* Voltage */
  | Volt => [{prefix: Unit, name: Volt, power: a.power}]
  /* Ampere */
  | Ampere => [{prefix: Unit, name: Ampere, power: a.power}]
  /* Resistance */
  | Ohm => [{prefix: Unit, name: Ohm, power: a.power}]
  /* Charge */
  | Coulomb => [{prefix: Unit, name: Coulomb, power: a.power}]
  /* Capacitance */
  | Farad => [{prefix: Unit, name: Farad, power: a.power}]
  /* Flux */
  | Weber => [{prefix: Unit, name: Weber, power: a.power}]
  /* Flux density */
  | Tesla => [{prefix: Unit, name: Tesla, power: a.power}]
  /* Inductance */
  | Henry => [{prefix: Unit, name: Henry, power: a.power}]
  /* Conductance */
  | Siemens => [{prefix: Unit, name: Siemens, power: a.power}]
  /* Substance */
  | Mole => [{prefix: Unit, name: Mole, power: a.power}]
  /* Frequency */
  | Hertz => [{prefix: Unit, name: Hertz, power: a.power}]
  /* Memory */
  | Bit
  | Byte => [{prefix: Unit, name: Byte, power: a.power}]
  /* Temperature */
  | Kelvin
  | Celsius
  | Fahrenheit => [{prefix: Unit, name: Kelvin, power: a.power}]
  }
