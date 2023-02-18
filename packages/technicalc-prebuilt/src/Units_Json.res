@@warning("-30")

open TechniCalcCalculator.Units_Types

type unitRow = {
  units: array<t>,
  primary: bool,
}

type section = {
  title: string,
  data: array<array<unitRow>>,
}

let json = [
  {
    title: "Temperature",
    data: [
      [{units: [{prefix: Unit, name: Kelvin, power: 1}], primary: true}],
      [{units: [{prefix: Unit, name: Celsius, power: 1}], primary: true}],
      [
        {
          units: [{prefix: Unit, name: Fahrenheit, power: 1}],
          primary: true,
        },
      ],
    ],
  },
  {
    title: "Time",
    data: [
      [
        {units: [{prefix: Milli, name: Second, power: 1}], primary: true},
        {
          units: [{prefix: Femto, name: Second, power: 1}],
          primary: false,
        },
        {units: [{prefix: Pico, name: Second, power: 1}], primary: false},
        {units: [{prefix: Nano, name: Second, power: 1}], primary: false},
        {
          units: [{prefix: Micro, name: Second, power: 1}],
          primary: false,
        },
        {units: [{prefix: Unit, name: Second, power: 1}], primary: true},
      ],
      [{units: [{prefix: Unit, name: Minute, power: 1}], primary: true}],
      [{units: [{prefix: Unit, name: Hour, power: 1}], primary: true}],
      [{units: [{prefix: Unit, name: Day, power: 1}], primary: true}],
      [{units: [{prefix: Unit, name: Week, power: 1}], primary: true}],
      [{units: [{prefix: Unit, name: Month, power: 1}], primary: true}],
      [{units: [{prefix: Unit, name: Year, power: 1}], primary: true}],
      [{units: [{prefix: Unit, name: Decade, power: 1}], primary: true}],
      [{units: [{prefix: Unit, name: Century, power: 1}], primary: true}],
      [
        {units: [{prefix: Nano, name: Hertz, power: 1}], primary: false},
        {units: [{prefix: Micro, name: Hertz, power: 1}], primary: false},
        {units: [{prefix: Milli, name: Hertz, power: 1}], primary: false},
        {units: [{prefix: Unit, name: Hertz, power: 1}], primary: true},
        {units: [{prefix: Kilo, name: Hertz, power: 1}], primary: false},
        {units: [{prefix: Mega, name: Hertz, power: 1}], primary: false},
        {units: [{prefix: Giga, name: Hertz, power: 1}], primary: false},
      ],
    ],
  },
  {
    title: "Length",
    data: [
      [
        {units: [{prefix: Femto, name: Meter, power: 1}], primary: false},
        {units: [{prefix: Pico, name: Meter, power: 1}], primary: false},
        {units: [{prefix: Nano, name: Meter, power: 1}], primary: false},
        {units: [{prefix: Micro, name: Meter, power: 1}], primary: false},
        {units: [{prefix: Milli, name: Meter, power: 1}], primary: true},
        {units: [{prefix: Centi, name: Meter, power: 1}], primary: true},
        {units: [{prefix: Unit, name: Meter, power: 1}], primary: true},
        {units: [{prefix: Kilo, name: Meter, power: 1}], primary: true},
      ],
      [
        {units: [{prefix: Unit, name: Inch, power: 1}], primary: true},
        {units: [{prefix: Unit, name: Foot, power: 1}], primary: true},
      ],
      [{units: [{prefix: Unit, name: Yard, power: 1}], primary: true}],
      [{units: [{prefix: Unit, name: Mile, power: 1}], primary: true}],
      [
        {
          units: [{prefix: Unit, name: ScandinavianMile, power: 1}],
          primary: true,
        },
        {
          units: [{prefix: Unit, name: NauticalMile, power: 1}],
          primary: true,
        },
      ],
      [
        {
          units: [{prefix: Unit, name: LightYear, power: 1}],
          primary: true,
        },
      ],
      [{units: [{prefix: Unit, name: Parsec, power: 1}], primary: true}],
      [
        {
          units: [{prefix: Unit, name: Angstrom, power: 1}],
          primary: true,
        },
      ],
    ],
  },
  {
    title: "Mass",
    data: [
      [
        {units: [{prefix: Femto, name: Gram, power: 1}], primary: false},
        {units: [{prefix: Pico, name: Gram, power: 1}], primary: false},
        {units: [{prefix: Nano, name: Gram, power: 1}], primary: false},
        {units: [{prefix: Micro, name: Gram, power: 1}], primary: false},
        {units: [{prefix: Milli, name: Gram, power: 1}], primary: false},
        {units: [{prefix: Unit, name: Gram, power: 1}], primary: true},
        {units: [{prefix: Kilo, name: Gram, power: 1}], primary: true},
      ],
      [{units: [{prefix: Unit, name: Tonne, power: 1}], primary: true}],
      [{units: [{prefix: Unit, name: Ounce, power: 1}], primary: true}],
      [{units: [{prefix: Unit, name: Pound, power: 1}], primary: true}],
      [{units: [{prefix: Unit, name: Stone, power: 1}], primary: true}],
    ],
  },
  {
    title: "Area",
    data: [
      [
        {units: [{prefix: Centi, name: Meter, power: 2}], primary: true},
        {units: [{prefix: Unit, name: Meter, power: 2}], primary: true},
      ],
      [
        {units: [{prefix: Unit, name: Foot, power: 2}], primary: true},
        {units: [{prefix: Unit, name: Inch, power: 2}], primary: true},
      ],
      [{units: [{prefix: Unit, name: Acre, power: 1}], primary: true}],
      [{units: [{prefix: Unit, name: Hectare, power: 1}], primary: true}],
    ],
  },
  {
    title: "Volume",
    data: [
      [
        {units: [{prefix: Centi, name: Meter, power: 3}], primary: true},
        {units: [{prefix: Unit, name: Meter, power: 3}], primary: true},
      ],
      [
        {units: [{prefix: Unit, name: Inch, power: 3}], primary: true},
        {units: [{prefix: Unit, name: Foot, power: 3}], primary: true},
      ],
      [
        {units: [{prefix: Milli, name: Liter, power: 1}], primary: true},
        {units: [{prefix: Centi, name: Liter, power: 1}], primary: true},
        {units: [{prefix: Unit, name: Liter, power: 1}], primary: true},
      ],
      [
        {units: [{prefix: Unit, name: Gallon, power: 1}], primary: true},
        {
          units: [{prefix: Unit, name: USGallon, power: 1}],
          primary: true,
        },
      ],
      [
        {units: [{prefix: Unit, name: Quart, power: 1}], primary: true},
        {units: [{prefix: Unit, name: USQuart, power: 1}], primary: true},
      ],
      [
        {units: [{prefix: Unit, name: Cup, power: 1}], primary: true},
        {units: [{prefix: Unit, name: USCup, power: 1}], primary: true},
      ],
      [
        {units: [{prefix: Unit, name: Pint, power: 1}], primary: true},
        {units: [{prefix: Unit, name: USPint, power: 1}], primary: true},
      ],
      [
        {
          units: [{prefix: Unit, name: Teaspoon, power: 1}],
          primary: true,
        },
        {
          units: [{prefix: Unit, name: Tablespoon, power: 1}],
          primary: true,
        },
        {
          units: [{prefix: Unit, name: USTeaspoon, power: 1}],
          primary: true,
        },
        {
          units: [{prefix: Unit, name: USTablespoon, power: 1}],
          primary: true,
        },
      ],
      [
        {
          units: [{prefix: Unit, name: FluidOunce, power: 1}],
          primary: true,
        },
      ],
    ],
  },
  {
    title: "Speed",
    data: [
      [
        {
          units: [{prefix: Unit, name: Mile, power: 1}, {prefix: Unit, name: Hour, power: -1}],
          primary: true,
        },
        {
          units: [{prefix: Kilo, name: Meter, power: 1}, {prefix: Unit, name: Hour, power: -1}],
          primary: true,
        },
      ],
      [
        {
          units: [{prefix: Unit, name: Meter, power: 1}, {prefix: Unit, name: Second, power: -1}],
          primary: true,
        },
      ],
      [{units: [{prefix: Unit, name: Knot, power: 1}], primary: true}],
    ],
  },
  {
    title: "Force",
    data: [
      [
        {units: [{prefix: Unit, name: Newton, power: 1}], primary: true},
        {
          units: [{prefix: Femto, name: Newton, power: 1}],
          primary: false,
        },
        {units: [{prefix: Pico, name: Newton, power: 1}], primary: false},
        {units: [{prefix: Nano, name: Newton, power: 1}], primary: false},
        {
          units: [{prefix: Micro, name: Newton, power: 1}],
          primary: false,
        },
        {
          units: [{prefix: Milli, name: Newton, power: 1}],
          primary: false,
        },
        {units: [{prefix: Kilo, name: Newton, power: 1}], primary: false},
        {units: [{prefix: Mega, name: Newton, power: 1}], primary: false},
        {units: [{prefix: Giga, name: Newton, power: 1}], primary: false},
        {units: [{prefix: Tera, name: Newton, power: 1}], primary: false},
        {units: [{prefix: Peta, name: Newton, power: 1}], primary: false},
      ],
      [
        {
          units: [{prefix: Unit, name: PoundForce, power: 1}],
          primary: true,
        },
      ],
    ],
  },
  {
    title: "Pressure",
    data: [
      [
        {
          units: [{prefix: Unit, name: Atmosphere, power: 1}],
          primary: true,
        },
      ],
      [
        {units: [{prefix: Unit, name: Pascal, power: 1}], primary: true},
        {
          units: [{prefix: Hecto, name: Pascal, power: 1}],
          primary: false,
        },
        {units: [{prefix: Kilo, name: Pascal, power: 1}], primary: false},
      ],
      [
        {units: [{prefix: Unit, name: Bar, power: 1}], primary: true},
        {units: [{prefix: Milli, name: Bar, power: 1}], primary: false},
      ],
    ],
  },
  {
    title: "Energy",
    data: [
      [{units: [{prefix: Unit, name: Joule, power: 1}], primary: true}],
      [
        {
          units: [{prefix: Unit, name: Watt, power: 1}, {prefix: Unit, name: Hour, power: 1}],
          primary: true,
        },
        {
          units: [{prefix: Kilo, name: Watt, power: 1}, {prefix: Unit, name: Hour, power: 1}],
          primary: true,
        },
      ],
      [{units: [{prefix: Unit, name: Calorie, power: 1}], primary: true}],
      [
        {
          units: [{prefix: Unit, name: ElectronVolt, power: 1}],
          primary: true,
        },
      ],
      [{units: [{prefix: Unit, name: BTU, power: 1}], primary: true}],
      [{units: [{prefix: Unit, name: Therm, power: 1}], primary: true}],
    ],
  },
  {
    title: "Electricity & Power",
    data: [
      [
        {units: [{prefix: Nano, name: Volt, power: 1}], primary: false},
        {units: [{prefix: Micro, name: Volt, power: 1}], primary: false},
        {units: [{prefix: Milli, name: Volt, power: 1}], primary: false},
        {units: [{prefix: Unit, name: Volt, power: 1}], primary: true},
        {units: [{prefix: Kilo, name: Volt, power: 1}], primary: false},
        {units: [{prefix: Mega, name: Volt, power: 1}], primary: false},
        {units: [{prefix: Giga, name: Volt, power: 1}], primary: false},
      ],
      [
        {units: [{prefix: Nano, name: Ampere, power: 1}], primary: false},
        {units: [{prefix: Micro, name: Ampere, power: 1}], primary: false},
        {units: [{prefix: Milli, name: Ampere, power: 1}], primary: false},
        {units: [{prefix: Unit, name: Ampere, power: 1}], primary: true},
        {units: [{prefix: Kilo, name: Ampere, power: 1}], primary: false},
        {units: [{prefix: Mega, name: Ampere, power: 1}], primary: false},
        {units: [{prefix: Giga, name: Ampere, power: 1}], primary: false},
      ],
      [
        {units: [{prefix: Nano, name: Ohm, power: 1}], primary: false},
        {units: [{prefix: Micro, name: Ohm, power: 1}], primary: false},
        {units: [{prefix: Milli, name: Ohm, power: 1}], primary: false},
        {units: [{prefix: Unit, name: Ohm, power: 1}], primary: true},
        {units: [{prefix: Kilo, name: Ohm, power: 1}], primary: false},
        {units: [{prefix: Mega, name: Ohm, power: 1}], primary: false},
        {units: [{prefix: Giga, name: Ohm, power: 1}], primary: false},
      ],
      [
        {units: [{prefix: Nano, name: Watt, power: 1}], primary: false},
        {units: [{prefix: Micro, name: Watt, power: 1}], primary: false},
        {units: [{prefix: Milli, name: Watt, power: 1}], primary: false},
        {units: [{prefix: Unit, name: Watt, power: 1}], primary: true},
        {units: [{prefix: Kilo, name: Watt, power: 1}], primary: false},
        {units: [{prefix: Mega, name: Watt, power: 1}], primary: false},
        {units: [{prefix: Giga, name: Watt, power: 1}], primary: false},
      ],
      [
        {units: [{prefix: Nano, name: Farad, power: 1}], primary: false},
        {units: [{prefix: Micro, name: Farad, power: 1}], primary: false},
        {units: [{prefix: Milli, name: Farad, power: 1}], primary: false},
        {units: [{prefix: Unit, name: Farad, power: 1}], primary: true},
        {units: [{prefix: Kilo, name: Farad, power: 1}], primary: false},
        {units: [{prefix: Mega, name: Farad, power: 1}], primary: false},
        {units: [{prefix: Giga, name: Farad, power: 1}], primary: false},
      ],
    ],
  },
  {
    title: "Science",
    data: [
      [{units: [{prefix: Unit, name: Mole, power: 1}], primary: true}],
      [
        {units: [{prefix: Nano, name: Coulomb, power: 1}], primary: false},
        {units: [{prefix: Micro, name: Coulomb, power: 1}], primary: false},
        {units: [{prefix: Milli, name: Coulomb, power: 1}], primary: false},
        {units: [{prefix: Unit, name: Coulomb, power: 1}], primary: true},
        {units: [{prefix: Kilo, name: Coulomb, power: 1}], primary: false},
        {units: [{prefix: Mega, name: Coulomb, power: 1}], primary: false},
        {units: [{prefix: Giga, name: Coulomb, power: 1}], primary: false},
      ],
      [
        {units: [{prefix: Nano, name: Tesla, power: 1}], primary: false},
        {units: [{prefix: Micro, name: Tesla, power: 1}], primary: false},
        {units: [{prefix: Milli, name: Tesla, power: 1}], primary: false},
        {units: [{prefix: Unit, name: Tesla, power: 1}], primary: true},
        {units: [{prefix: Kilo, name: Tesla, power: 1}], primary: false},
        {units: [{prefix: Mega, name: Tesla, power: 1}], primary: false},
        {units: [{prefix: Giga, name: Tesla, power: 1}], primary: false},
      ],
      [
        {units: [{prefix: Nano, name: Henry, power: 1}], primary: false},
        {units: [{prefix: Micro, name: Henry, power: 1}], primary: false},
        {units: [{prefix: Milli, name: Henry, power: 1}], primary: false},
        {units: [{prefix: Unit, name: Henry, power: 1}], primary: true},
        {units: [{prefix: Kilo, name: Henry, power: 1}], primary: false},
        {units: [{prefix: Mega, name: Henry, power: 1}], primary: false},
        {units: [{prefix: Giga, name: Henry, power: 1}], primary: false},
      ],
    ],
  },
  {
    title: "Memory",
    data: [
      [
        {units: [{prefix: Unit, name: Byte, power: 1}], primary: true},
        {units: [{prefix: Kibi, name: Byte, power: 1}], primary: true},
        {units: [{prefix: Mebi, name: Byte, power: 1}], primary: true},
        {units: [{prefix: Gibi, name: Byte, power: 1}], primary: true},
        {units: [{prefix: Tebi, name: Byte, power: 1}], primary: false},
        {units: [{prefix: Pebi, name: Byte, power: 1}], primary: false},
        {units: [{prefix: Exbi, name: Byte, power: 1}], primary: false},
        {units: [{prefix: Kilo, name: Byte, power: 1}], primary: false},
        {units: [{prefix: Mega, name: Byte, power: 1}], primary: false},
        {units: [{prefix: Giga, name: Byte, power: 1}], primary: false},
        {units: [{prefix: Tera, name: Byte, power: 1}], primary: false},
        {units: [{prefix: Peta, name: Byte, power: 1}], primary: false},
        {units: [{prefix: Exa, name: Byte, power: 1}], primary: false},
      ],
      [
        {units: [{prefix: Unit, name: Bit, power: 1}], primary: true},
        {units: [{prefix: Kibi, name: Bit, power: 1}], primary: true},
        {units: [{prefix: Mebi, name: Bit, power: 1}], primary: true},
        {units: [{prefix: Gibi, name: Bit, power: 1}], primary: true},
        {units: [{prefix: Tebi, name: Bit, power: 1}], primary: false},
        {units: [{prefix: Pebi, name: Bit, power: 1}], primary: false},
        {units: [{prefix: Exbi, name: Bit, power: 1}], primary: false},
        {units: [{prefix: Kilo, name: Bit, power: 1}], primary: false},
        {units: [{prefix: Mega, name: Bit, power: 1}], primary: false},
        {units: [{prefix: Giga, name: Bit, power: 1}], primary: false},
        {units: [{prefix: Tera, name: Bit, power: 1}], primary: false},
        {units: [{prefix: Peta, name: Bit, power: 1}], primary: false},
        {units: [{prefix: Exa, name: Bit, power: 1}], primary: false},
      ],
    ],
  },
]
