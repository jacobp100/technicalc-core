@@warning("-30")

open TechniCalcCalculator.Unit_Types

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
    title: "Time",
    data: [
      [
        {units: [{prefix: Milli, type_: Second, power: 1}], primary: true},
        {
          units: [{prefix: Femto, type_: Second, power: 1}],
          primary: false,
        },
        {units: [{prefix: Pico, type_: Second, power: 1}], primary: false},
        {units: [{prefix: Nano, type_: Second, power: 1}], primary: false},
        {
          units: [{prefix: Micro, type_: Second, power: 1}],
          primary: false,
        },
        {units: [{prefix: Unit, type_: Second, power: 1}], primary: true},
      ],
      [{units: [{prefix: Unit, type_: Minute, power: 1}], primary: true}],
      [{units: [{prefix: Unit, type_: Hour, power: 1}], primary: true}],
      [{units: [{prefix: Unit, type_: Day, power: 1}], primary: true}],
      [{units: [{prefix: Unit, type_: Week, power: 1}], primary: true}],
      [{units: [{prefix: Unit, type_: Month, power: 1}], primary: true}],
      [{units: [{prefix: Unit, type_: Year, power: 1}], primary: true}],
      [{units: [{prefix: Unit, type_: Decade, power: 1}], primary: true}],
      [{units: [{prefix: Unit, type_: Century, power: 1}], primary: true}],
    ],
  },
  {
    title: "Length",
    data: [
      [
        {units: [{prefix: Femto, type_: Meter, power: 1}], primary: false},
        {units: [{prefix: Pico, type_: Meter, power: 1}], primary: false},
        {units: [{prefix: Nano, type_: Meter, power: 1}], primary: false},
        {units: [{prefix: Micro, type_: Meter, power: 1}], primary: false},
        {units: [{prefix: Milli, type_: Meter, power: 1}], primary: true},
        {units: [{prefix: Centi, type_: Meter, power: 1}], primary: true},
        {units: [{prefix: Unit, type_: Meter, power: 1}], primary: true},
        {units: [{prefix: Kilo, type_: Meter, power: 1}], primary: true},
      ],
      [
        {units: [{prefix: Unit, type_: Inch, power: 1}], primary: true},
        {units: [{prefix: Unit, type_: Foot, power: 1}], primary: true},
      ],
      [{units: [{prefix: Unit, type_: Yard, power: 1}], primary: true}],
      [{units: [{prefix: Unit, type_: Mile, power: 1}], primary: true}],
      [
        {
          units: [{prefix: Unit, type_: NauticalMile, power: 1}],
          primary: true,
        },
      ],
      [
        {
          units: [{prefix: Unit, type_: LightYear, power: 1}],
          primary: true,
        },
      ],
      [{units: [{prefix: Unit, type_: Parsec, power: 1}], primary: true}],
      [
        {
          units: [{prefix: Unit, type_: Angstrom, power: 1}],
          primary: true,
        },
      ],
    ],
  },
  {
    title: "Mass",
    data: [
      [
        {units: [{prefix: Femto, type_: Gram, power: 1}], primary: false},
        {units: [{prefix: Pico, type_: Gram, power: 1}], primary: false},
        {units: [{prefix: Nano, type_: Gram, power: 1}], primary: false},
        {units: [{prefix: Micro, type_: Gram, power: 1}], primary: false},
        {units: [{prefix: Milli, type_: Gram, power: 1}], primary: false},
        {units: [{prefix: Unit, type_: Gram, power: 1}], primary: true},
        {units: [{prefix: Kilo, type_: Gram, power: 1}], primary: true},
      ],
      [{units: [{prefix: Unit, type_: Tonne, power: 1}], primary: true}],
      [{units: [{prefix: Unit, type_: Ounce, power: 1}], primary: true}],
      [{units: [{prefix: Unit, type_: Pound, power: 1}], primary: true}],
      [{units: [{prefix: Unit, type_: Stone, power: 1}], primary: true}],
    ],
  },
  {
    title: "Area",
    data: [
      [
        {units: [{prefix: Centi, type_: Meter, power: 2}], primary: true},
        {units: [{prefix: Unit, type_: Meter, power: 2}], primary: true},
      ],
      [
        {units: [{prefix: Unit, type_: Foot, power: 2}], primary: true},
        {units: [{prefix: Unit, type_: Inch, power: 2}], primary: true},
      ],
      [{units: [{prefix: Unit, type_: Acre, power: 1}], primary: true}],
      [{units: [{prefix: Unit, type_: Hectare, power: 1}], primary: true}],
    ],
  },
  {
    title: "Volume",
    data: [
      [
        {units: [{prefix: Centi, type_: Meter, power: 3}], primary: true},
        {units: [{prefix: Unit, type_: Meter, power: 3}], primary: true},
      ],
      [
        {units: [{prefix: Unit, type_: Inch, power: 3}], primary: true},
        {units: [{prefix: Unit, type_: Foot, power: 3}], primary: true},
      ],
      [
        {units: [{prefix: Milli, type_: Liter, power: 1}], primary: true},
        {units: [{prefix: Centi, type_: Liter, power: 1}], primary: true},
        {units: [{prefix: Unit, type_: Liter, power: 1}], primary: true},
      ],
      [
        {units: [{prefix: Unit, type_: Gallon, power: 1}], primary: true},
        {
          units: [{prefix: Unit, type_: USGallon, power: 1}],
          primary: true,
        },
      ],
      [
        {units: [{prefix: Unit, type_: Quart, power: 1}], primary: true},
        {units: [{prefix: Unit, type_: USQuart, power: 1}], primary: true},
      ],
      [
        {units: [{prefix: Unit, type_: Cup, power: 1}], primary: true},
        {units: [{prefix: Unit, type_: USCup, power: 1}], primary: true},
      ],
      [
        {units: [{prefix: Unit, type_: Pint, power: 1}], primary: true},
        {units: [{prefix: Unit, type_: USPint, power: 1}], primary: true},
      ],
      [
        {
          units: [{prefix: Unit, type_: Teaspoon, power: 1}],
          primary: true,
        },
        {
          units: [{prefix: Unit, type_: Tablespoon, power: 1}],
          primary: true,
        },
        {
          units: [{prefix: Unit, type_: USTeaspoon, power: 1}],
          primary: true,
        },
        {
          units: [{prefix: Unit, type_: USTablespoon, power: 1}],
          primary: true,
        },
      ],
      [
        {
          units: [{prefix: Unit, type_: FluidOunce, power: 1}],
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
          units: [{prefix: Unit, type_: Mile, power: 1}, {prefix: Unit, type_: Hour, power: -1}],
          primary: true,
        },
        {
          units: [{prefix: Kilo, type_: Meter, power: 1}, {prefix: Unit, type_: Hour, power: -1}],
          primary: true,
        },
      ],
      [
        {
          units: [{prefix: Unit, type_: Meter, power: 1}, {prefix: Unit, type_: Second, power: -1}],
          primary: true,
        },
      ],
      [{units: [{prefix: Unit, type_: Knot, power: 1}], primary: true}],
    ],
  },
  {
    title: "Force",
    data: [
      [
        {units: [{prefix: Unit, type_: Newton, power: 1}], primary: true},
        {
          units: [{prefix: Femto, type_: Newton, power: 1}],
          primary: false,
        },
        {units: [{prefix: Pico, type_: Newton, power: 1}], primary: false},
        {units: [{prefix: Nano, type_: Newton, power: 1}], primary: false},
        {
          units: [{prefix: Micro, type_: Newton, power: 1}],
          primary: false,
        },
        {
          units: [{prefix: Milli, type_: Newton, power: 1}],
          primary: false,
        },
        {units: [{prefix: Kilo, type_: Newton, power: 1}], primary: false},
        {units: [{prefix: Mega, type_: Newton, power: 1}], primary: false},
        {units: [{prefix: Giga, type_: Newton, power: 1}], primary: false},
        {units: [{prefix: Tera, type_: Newton, power: 1}], primary: false},
        {units: [{prefix: Peta, type_: Newton, power: 1}], primary: false},
      ],
      [
        {
          units: [{prefix: Unit, type_: PoundForce, power: 1}],
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
          units: [{prefix: Unit, type_: Atmosphere, power: 1}],
          primary: true,
        },
      ],
      [
        {units: [{prefix: Unit, type_: Pascal, power: 1}], primary: true},
        {
          units: [{prefix: Hecto, type_: Pascal, power: 1}],
          primary: false,
        },
        {units: [{prefix: Kilo, type_: Pascal, power: 1}], primary: false},
      ],
      [
        {units: [{prefix: Unit, type_: Bar, power: 1}], primary: true},
        {units: [{prefix: Milli, type_: Bar, power: 1}], primary: false},
      ],
    ],
  },
  {
    title: "Energy",
    data: [
      [{units: [{prefix: Unit, type_: Joule, power: 1}], primary: true}],
      [
        {
          units: [{prefix: Unit, type_: Watt, power: 1}, {prefix: Unit, type_: Hour, power: 1}],
          primary: true,
        },
        {
          units: [{prefix: Kilo, type_: Watt, power: 1}, {prefix: Unit, type_: Hour, power: 1}],
          primary: true,
        },
      ],
      [{units: [{prefix: Unit, type_: Calorie, power: 1}], primary: true}],
      [
        {
          units: [{prefix: Unit, type_: ElectronVolt, power: 1}],
          primary: true,
        },
      ],
      [{units: [{prefix: Unit, type_: BTU, power: 1}], primary: true}],
      [{units: [{prefix: Unit, type_: Therm, power: 1}], primary: true}],
    ],
  },
  {
    title: "Power",
    data: [
      [
        {units: [{prefix: Nano, type_: Joule, power: 1}], primary: false},
        {units: [{prefix: Micro, type_: Joule, power: 1}], primary: false},
        {units: [{prefix: Milli, type_: Joule, power: 1}], primary: false},
        {units: [{prefix: Unit, type_: Watt, power: 1}], primary: true},
        {units: [{prefix: Kilo, type_: Joule, power: 1}], primary: false},
        {units: [{prefix: Mega, type_: Joule, power: 1}], primary: false},
        {units: [{prefix: Giga, type_: Joule, power: 1}], primary: false},
      ],
    ],
  },
  {
    title: "Memory",
    data: [
      [
        {units: [{prefix: Unit, type_: Byte, power: 1}], primary: true},
        {units: [{prefix: Kibi, type_: Byte, power: 1}], primary: true},
        {units: [{prefix: Mebi, type_: Byte, power: 1}], primary: true},
        {units: [{prefix: Gibi, type_: Byte, power: 1}], primary: true},
        {units: [{prefix: Tebi, type_: Byte, power: 1}], primary: false},
        {units: [{prefix: Pebi, type_: Byte, power: 1}], primary: false},
        {units: [{prefix: Exbi, type_: Byte, power: 1}], primary: false},
        {units: [{prefix: Kilo, type_: Byte, power: 1}], primary: false},
        {units: [{prefix: Mega, type_: Byte, power: 1}], primary: false},
        {units: [{prefix: Giga, type_: Byte, power: 1}], primary: false},
        {units: [{prefix: Tera, type_: Byte, power: 1}], primary: false},
        {units: [{prefix: Peta, type_: Byte, power: 1}], primary: false},
        {units: [{prefix: Exa, type_: Byte, power: 1}], primary: false},
      ],
      [
        {units: [{prefix: Unit, type_: Bit, power: 1}], primary: true},
        {units: [{prefix: Kibi, type_: Bit, power: 1}], primary: true},
        {units: [{prefix: Mebi, type_: Bit, power: 1}], primary: true},
        {units: [{prefix: Gibi, type_: Bit, power: 1}], primary: true},
        {units: [{prefix: Tebi, type_: Bit, power: 1}], primary: false},
        {units: [{prefix: Pebi, type_: Bit, power: 1}], primary: false},
        {units: [{prefix: Exbi, type_: Bit, power: 1}], primary: false},
        {units: [{prefix: Kilo, type_: Bit, power: 1}], primary: false},
        {units: [{prefix: Mega, type_: Bit, power: 1}], primary: false},
        {units: [{prefix: Giga, type_: Bit, power: 1}], primary: false},
        {units: [{prefix: Tera, type_: Bit, power: 1}], primary: false},
        {units: [{prefix: Peta, type_: Bit, power: 1}], primary: false},
        {units: [{prefix: Exa, type_: Bit, power: 1}], primary: false},
      ],
    ],
  },
  {
    title: "Temperature",
    data: [
      [{units: [{prefix: Unit, type_: Kelvin, power: 1}], primary: true}],
      [{units: [{prefix: Unit, type_: Celsius, power: 1}], primary: true}],
      [
        {
          units: [{prefix: Unit, type_: Fahrenheit, power: 1}],
          primary: true,
        },
      ],
    ],
  },
]
