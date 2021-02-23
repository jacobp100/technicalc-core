[@warning "-30"];

open TechniCalcCalculator.Unit_Types;

type section = {
  title: string,
  data: array(array(unitRow)),
}
and unitRow = {
  units: array(unitPart),
  primary: bool,
};

let json = [|
  {
    title: "Time",
    data: [|
      [|
        {units: [|{prefix: Milli, unit: Second, power: 1}|], primary: true},
        {
          units: [|{prefix: Femto, unit: Second, power: 1}|],
          primary: false,
        },
        {units: [|{prefix: Pico, unit: Second, power: 1}|], primary: false},
        {units: [|{prefix: Nano, unit: Second, power: 1}|], primary: false},
        {
          units: [|{prefix: Micro, unit: Second, power: 1}|],
          primary: false,
        },
        {units: [|{prefix: Unit, unit: Second, power: 1}|], primary: true},
      |],
      [|
        {units: [|{prefix: Unit, unit: Minute, power: 1}|], primary: true},
      |],
      [|{units: [|{prefix: Unit, unit: Hour, power: 1}|], primary: true}|],
      [|{units: [|{prefix: Unit, unit: Day, power: 1}|], primary: true}|],
      [|{units: [|{prefix: Unit, unit: Week, power: 1}|], primary: true}|],
      [|
        {units: [|{prefix: Unit, unit: Month, power: 1}|], primary: true},
      |],
      [|{units: [|{prefix: Unit, unit: Year, power: 1}|], primary: true}|],
      [|
        {units: [|{prefix: Unit, unit: Decade, power: 1}|], primary: true},
      |],
      [|
        {units: [|{prefix: Unit, unit: Century, power: 1}|], primary: true},
      |],
    |],
  },
  {
    title: "Length",
    data: [|
      [|
        {units: [|{prefix: Femto, unit: Meter, power: 1}|], primary: false},
        {units: [|{prefix: Pico, unit: Meter, power: 1}|], primary: false},
        {units: [|{prefix: Nano, unit: Meter, power: 1}|], primary: false},
        {units: [|{prefix: Micro, unit: Meter, power: 1}|], primary: false},
        {units: [|{prefix: Milli, unit: Meter, power: 1}|], primary: true},
        {units: [|{prefix: Centi, unit: Meter, power: 1}|], primary: true},
        {units: [|{prefix: Unit, unit: Meter, power: 1}|], primary: true},
        {units: [|{prefix: Kilo, unit: Meter, power: 1}|], primary: true},
      |],
      [|
        {units: [|{prefix: Unit, unit: Inch, power: 1}|], primary: true},
        {units: [|{prefix: Unit, unit: Foot, power: 1}|], primary: true},
      |],
      [|{units: [|{prefix: Unit, unit: Yard, power: 1}|], primary: true}|],
      [|{units: [|{prefix: Unit, unit: Mile, power: 1}|], primary: true}|],
      [|
        {
          units: [|{prefix: Unit, unit: NauticalMile, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          units: [|{prefix: Unit, unit: LightYear, power: 1}|],
          primary: true,
        },
      |],
      [|
        {units: [|{prefix: Unit, unit: Parsec, power: 1}|], primary: true},
      |],
      [|
        {
          units: [|{prefix: Unit, unit: Angstrom, power: 1}|],
          primary: true,
        },
      |],
    |],
  },
  {
    title: "Mass",
    data: [|
      [|
        {units: [|{prefix: Femto, unit: Gram, power: 1}|], primary: false},
        {units: [|{prefix: Pico, unit: Gram, power: 1}|], primary: false},
        {units: [|{prefix: Nano, unit: Gram, power: 1}|], primary: false},
        {units: [|{prefix: Micro, unit: Gram, power: 1}|], primary: false},
        {units: [|{prefix: Milli, unit: Gram, power: 1}|], primary: false},
        {units: [|{prefix: Unit, unit: Gram, power: 1}|], primary: true},
        {units: [|{prefix: Kilo, unit: Gram, power: 1}|], primary: true},
      |],
      [|
        {units: [|{prefix: Unit, unit: Tonne, power: 1}|], primary: true},
      |],
      [|
        {units: [|{prefix: Unit, unit: Ounce, power: 1}|], primary: true},
      |],
      [|
        {units: [|{prefix: Unit, unit: Pound, power: 1}|], primary: true},
      |],
      [|
        {units: [|{prefix: Unit, unit: Stone, power: 1}|], primary: true},
      |],
    |],
  },
  {
    title: "Area",
    data: [|
      [|
        {units: [|{prefix: Centi, unit: Meter, power: 2}|], primary: true},
        {units: [|{prefix: Unit, unit: Meter, power: 2}|], primary: true},
      |],
      [|
        {units: [|{prefix: Unit, unit: Foot, power: 2}|], primary: true},
        {units: [|{prefix: Unit, unit: Inch, power: 2}|], primary: true},
      |],
      [|{units: [|{prefix: Unit, unit: Acre, power: 1}|], primary: true}|],
      [|
        {units: [|{prefix: Unit, unit: Hectare, power: 1}|], primary: true},
      |],
    |],
  },
  {
    title: "Volume",
    data: [|
      [|
        {units: [|{prefix: Centi, unit: Meter, power: 3}|], primary: true},
        {units: [|{prefix: Unit, unit: Meter, power: 3}|], primary: true},
      |],
      [|
        {units: [|{prefix: Unit, unit: Inch, power: 3}|], primary: true},
        {units: [|{prefix: Unit, unit: Foot, power: 3}|], primary: true},
      |],
      [|
        {units: [|{prefix: Milli, unit: Liter, power: 1}|], primary: true},
        {units: [|{prefix: Centi, unit: Liter, power: 1}|], primary: true},
        {units: [|{prefix: Unit, unit: Liter, power: 1}|], primary: true},
      |],
      [|
        {units: [|{prefix: Unit, unit: Gallon, power: 1}|], primary: true},
        {
          units: [|{prefix: Unit, unit: USGallon, power: 1}|],
          primary: true,
        },
      |],
      [|
        {units: [|{prefix: Unit, unit: Quart, power: 1}|], primary: true},
      |],
      [|
        {units: [|{prefix: Unit, unit: Cup, power: 1}|], primary: true},
        {units: [|{prefix: Unit, unit: USCup, power: 1}|], primary: true},
      |],
      [|
        {
          units: [|{prefix: Unit, unit: Teaspoon, power: 1}|],
          primary: true,
        },
        {
          units: [|{prefix: Unit, unit: Tablespoon, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          units: [|{prefix: Unit, unit: FluidOunce, power: 1}|],
          primary: true,
        },
      |],
    |],
  },
  {
    title: "Speed",
    data: [|
      [|
        {
          units: [|
            {prefix: Unit, unit: Mile, power: 1},
            {prefix: Unit, unit: Hour, power: (-1)},
          |],
          primary: true,
        },
        {
          units: [|
            {prefix: Kilo, unit: Meter, power: 1},
            {prefix: Unit, unit: Hour, power: (-1)},
          |],
          primary: true,
        },
      |],
      [|
        {
          units: [|
            {prefix: Unit, unit: Meter, power: 1},
            {prefix: Unit, unit: Second, power: (-1)},
          |],
          primary: true,
        },
      |],
      [|{units: [|{prefix: Unit, unit: Knot, power: 1}|], primary: true}|],
    |],
  },
  {
    title: "Force",
    data: [|
      [|
        {units: [|{prefix: Unit, unit: Newton, power: 1}|], primary: true},
        {
          units: [|{prefix: Femto, unit: Newton, power: 1}|],
          primary: false,
        },
        {units: [|{prefix: Pico, unit: Newton, power: 1}|], primary: false},
        {units: [|{prefix: Nano, unit: Newton, power: 1}|], primary: false},
        {
          units: [|{prefix: Micro, unit: Newton, power: 1}|],
          primary: false,
        },
        {
          units: [|{prefix: Milli, unit: Newton, power: 1}|],
          primary: false,
        },
        {units: [|{prefix: Kilo, unit: Newton, power: 1}|], primary: false},
        {units: [|{prefix: Mega, unit: Newton, power: 1}|], primary: false},
        {units: [|{prefix: Giga, unit: Newton, power: 1}|], primary: false},
        {units: [|{prefix: Tera, unit: Newton, power: 1}|], primary: false},
        {units: [|{prefix: Peta, unit: Newton, power: 1}|], primary: false},
      |],
      [|
        {
          units: [|{prefix: Unit, unit: PoundForce, power: 1}|],
          primary: true,
        },
      |],
    |],
  },
  {
    title: "Pressure",
    data: [|
      [|
        {
          units: [|{prefix: Unit, unit: Atmosphere, power: 1}|],
          primary: true,
        },
      |],
      [|
        {units: [|{prefix: Unit, unit: Pascal, power: 1}|], primary: true},
        {
          units: [|{prefix: Hecto, unit: Pascal, power: 1}|],
          primary: false,
        },
        {units: [|{prefix: Kilo, unit: Pascal, power: 1}|], primary: false},
      |],
      [|
        {units: [|{prefix: Unit, unit: Bar, power: 1}|], primary: true},
        {units: [|{prefix: Milli, unit: Bar, power: 1}|], primary: false},
      |],
    |],
  },
  {
    title: "Energy",
    data: [|
      [|
        {units: [|{prefix: Unit, unit: Joule, power: 1}|], primary: true},
      |],
      [|
        {
          units: [|
            {prefix: Unit, unit: Watt, power: 1},
            {prefix: Unit, unit: Hour, power: 1},
          |],
          primary: true,
        },
        {
          units: [|
            {prefix: Kilo, unit: Watt, power: 1},
            {prefix: Unit, unit: Hour, power: 1},
          |],
          primary: true,
        },
      |],
      [|
        {units: [|{prefix: Unit, unit: Calorie, power: 1}|], primary: true},
      |],
      [|
        {
          units: [|{prefix: Unit, unit: ElectronVolt, power: 1}|],
          primary: true,
        },
      |],
      [|{units: [|{prefix: Unit, unit: BTU, power: 1}|], primary: true}|],
      [|
        {units: [|{prefix: Unit, unit: Therm, power: 1}|], primary: true},
      |],
    |],
  },
  {
    title: "Power",
    data: [|
      [|
        {units: [|{prefix: Nano, unit: Joule, power: 1}|], primary: false},
        {units: [|{prefix: Micro, unit: Joule, power: 1}|], primary: false},
        {units: [|{prefix: Milli, unit: Joule, power: 1}|], primary: false},
        {units: [|{prefix: Unit, unit: Watt, power: 1}|], primary: true},
        {units: [|{prefix: Kilo, unit: Joule, power: 1}|], primary: false},
        {units: [|{prefix: Mega, unit: Joule, power: 1}|], primary: false},
        {units: [|{prefix: Giga, unit: Joule, power: 1}|], primary: false},
      |],
    |],
  },
  {
    title: "Memory",
    data: [|
      [|
        {units: [|{prefix: Unit, unit: Byte, power: 1}|], primary: true},
        {units: [|{prefix: Kibi, unit: Byte, power: 1}|], primary: true},
        {units: [|{prefix: Mebi, unit: Byte, power: 1}|], primary: true},
        {units: [|{prefix: Gibi, unit: Byte, power: 1}|], primary: true},
        {units: [|{prefix: Tebi, unit: Byte, power: 1}|], primary: false},
        {units: [|{prefix: Pebi, unit: Byte, power: 1}|], primary: false},
        {units: [|{prefix: Kilo, unit: Byte, power: 1}|], primary: false},
        {units: [|{prefix: Mega, unit: Byte, power: 1}|], primary: false},
        {units: [|{prefix: Giga, unit: Byte, power: 1}|], primary: false},
        {units: [|{prefix: Tera, unit: Byte, power: 1}|], primary: false},
        {units: [|{prefix: Peta, unit: Byte, power: 1}|], primary: false},
      |],
      [|
        {units: [|{prefix: Unit, unit: Bit, power: 1}|], primary: true},
        {units: [|{prefix: Kibi, unit: Bit, power: 1}|], primary: true},
        {units: [|{prefix: Mebi, unit: Bit, power: 1}|], primary: true},
        {units: [|{prefix: Gibi, unit: Bit, power: 1}|], primary: true},
        {units: [|{prefix: Tebi, unit: Bit, power: 1}|], primary: false},
        {units: [|{prefix: Pebi, unit: Bit, power: 1}|], primary: false},
        {units: [|{prefix: Kilo, unit: Bit, power: 1}|], primary: false},
        {units: [|{prefix: Mega, unit: Bit, power: 1}|], primary: false},
        {units: [|{prefix: Giga, unit: Bit, power: 1}|], primary: false},
        {units: [|{prefix: Tera, unit: Bit, power: 1}|], primary: false},
        {units: [|{prefix: Peta, unit: Bit, power: 1}|], primary: false},
      |],
    |],
  },
  {
    title: "Temperature",
    data: [|
      [|
        {units: [|{prefix: Unit, unit: Kelvin, power: 1}|], primary: true},
      |],
      [|
        {units: [|{prefix: Unit, unit: Celsius, power: 1}|], primary: true},
      |],
      [|
        {
          units: [|{prefix: Unit, unit: Fahrenheit, power: 1}|],
          primary: true,
        },
      |],
    |],
  },
|];
