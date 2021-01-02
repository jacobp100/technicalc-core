[@warning "-30"];

open TechniCalcCalculator.Unit_Types;

type section = {
  title: string,
  data: array(array(unitRow)),
}
and unitRow = {
  title: string,
  units: array(unitPart),
  primary: bool,
};

let prefixToString = (prefix: prefix) =>
  switch (prefix) {
  | Femto => Some("Femto")
  | Pico => Some("Pico")
  | Nano => Some("Nano")
  | Micro => Some("Micro")
  | Milli => Some("Milli")
  | Centi => Some("Centi")
  | Deci => Some("Deci")
  | Unit => None
  | Deca => Some("Deca")
  | Hecto => Some("Hecto")
  | Kilo => Some("Kilo")
  | Mega => Some("Mega")
  | Giga => Some("Giga")
  | Tera => Some("Tera")
  | Peta => Some("Peta")
  | Kibi => Some("Kibi")
  | Mebi => Some("Mebi")
  | Gibi => Some("Gibi")
  | Tebi => Some("Tebi")
  | Pebi => Some("Pebi")
  };

let prefixValueToMml = (prefix: prefix) =>
  switch (prefix) {
  | Femto => Some("<msup><mn>10</mn><mn>-15</mn></msup>")
  | Pico => Some("<msup><mn>10</mn><mn>-12</mn></msup>")
  | Nano => Some("<msup><mn>10</mn><mn>-9</mn></msup>")
  | Micro => Some("<msup><mn>10</mn><mn>-6</mn></msup>")
  | Milli => Some("<msup><mn>10</mn><mn>-3</mn></msup>")
  | Centi => Some("<msup><mn>10</mn><mn>-2</mn></msup>")
  | Deci => Some("<msup><mn>10</mn><mn>-1</mn></msup>")
  | Unit => None
  | Deca => Some("<msup><mn>10</mn><mn>1</mn></msup>")
  | Hecto => Some("<msup><mn>10</mn><mn>2</mn></msup>")
  | Kilo => Some("<msup><mn>10</mn><mn>3</mn></msup>")
  | Mega => Some("<msup><mn>10</mn><mn>6</mn></msup>")
  | Giga => Some("<msup><mn>10</mn><mn>9</mn></msup>")
  | Tera => Some("<msup><mn>10</mn><mn>12</mn></msup>")
  | Peta => Some("<msup><mn>10</mn><mn>15</mn></msup>")
  | Kibi => Some("<msup><mn>2</mn><mn>10</mn></msup>")
  | Mebi => Some("<msup><mn>2</mn><mn>20</mn></msup>")
  | Gibi => Some("<msup><mn>2</mn><mn>30</mn></msup>")
  | Tebi => Some("<msup><mn>2</mn><mn>40</mn></msup>")
  | Pebi => Some("<msup><mn>2</mn><mn>50</mn></msup>")
  };

let unitToString = (unit: unitType) =>
  switch (unit) {
  | Second => "Second"
  | Minute => "Minute"
  | Hour => "Hour"
  | Day => "Day"
  | Week => "Week"
  | Month => "Month"
  | Year => "Year"
  | Decade => "Decade"
  | Century => "Century"
  | Meter => "Meter"
  | Inch => "Inch"
  | Foot => "Foot"
  | Yard => "Yard"
  | Mile => "Mile"
  | NauticalMile => "Nautical Mile"
  | LightYear => "Light Year"
  | Parsec => "Parsec"
  | Angstrom => "Angstrom"
  | Gram => "Gram"
  | Tonne => "Tonne"
  | Ounce => "Ounce"
  | Pound => "Pound"
  | Stone => "Stone"
  | Acre => "Acre"
  | Hectare => "Hectare"
  | Liter => "Liter"
  | Gallon => "Gallon"
  | USGallon => "US Gallon"
  | Quart => "Quart"
  | Cup => "Cup"
  | USCup => "USCup"
  | Teaspoon => "Teaspoon"
  | Tablespoon => "Tablespoon"
  | FluidOunce => "Fluid Ounce"
  | Knot => "Knot"
  | Newton => "Newton"
  | PoundForce => "Pound Force"
  | Pascal => "Pascal"
  | Atmosphere => "Atmosphere"
  | Bar => "Bar"
  | Joule => "Joule"
  | Calorie => "Calorie"
  | ElectronVolt => "Electron Volt"
  | BTU => "BTU"
  | Therm => "Therm"
  | Watt => "Watt"
  | Horsepower => "Horsepower"
  | MetricHorsepower => "Metric Horsepower"
  | Bit => "Bit"
  | Byte => "Byte"
  | Kelvin => "Kelvin"
  | Celsius => "Celsius"
  | Fahrenheit => "Fahrenheit"
  };

let prefixes = [|
  Unit,
  Femto,
  Pico,
  Nano,
  Micro,
  Milli,
  Centi,
  Deci,
  Deca,
  Hecto,
  Kilo,
  Mega,
  Giga,
  Tera,
  Peta,
  Kibi,
  Mebi,
  Gibi,
  Tebi,
  Pebi,
|];

let units = [|
  {
    title: "Time",
    data: [|
      [|
        {
          title: "Millisecond",
          units: [|{prefix: Milli, unit: Second, power: 1}|],
          primary: true,
        },
        {
          title: "Femtosecond",
          units: [|{prefix: Femto, unit: Second, power: 1}|],
          primary: false,
        },
        {
          title: "Picosecond",
          units: [|{prefix: Pico, unit: Second, power: 1}|],
          primary: false,
        },
        {
          title: "Nanosecond",
          units: [|{prefix: Nano, unit: Second, power: 1}|],
          primary: false,
        },
        {
          title: "Microsecond",
          units: [|{prefix: Micro, unit: Second, power: 1}|],
          primary: false,
        },
        {
          title: "Second",
          units: [|{prefix: Unit, unit: Second, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Minute",
          units: [|{prefix: Unit, unit: Minute, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Hour",
          units: [|{prefix: Unit, unit: Hour, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Day",
          units: [|{prefix: Unit, unit: Day, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Week",
          units: [|{prefix: Unit, unit: Week, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Month",
          units: [|{prefix: Unit, unit: Month, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Year",
          units: [|{prefix: Unit, unit: Year, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Decade",
          units: [|{prefix: Unit, unit: Decade, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Century",
          units: [|{prefix: Unit, unit: Century, power: 1}|],
          primary: true,
        },
      |],
    |],
  },
  {
    title: "Length",
    data: [|
      [|
        {
          title: "Femtometer",
          units: [|{prefix: Femto, unit: Meter, power: 1}|],
          primary: false,
        },
        {
          title: "Picometer",
          units: [|{prefix: Pico, unit: Meter, power: 1}|],
          primary: false,
        },
        {
          title: "Nanometer",
          units: [|{prefix: Nano, unit: Meter, power: 1}|],
          primary: false,
        },
        {
          title: "Micrometer",
          units: [|{prefix: Micro, unit: Meter, power: 1}|],
          primary: false,
        },
        {
          title: "Millimeter",
          units: [|{prefix: Milli, unit: Meter, power: 1}|],
          primary: true,
        },
        {
          title: "Centimeter",
          units: [|{prefix: Centi, unit: Meter, power: 1}|],
          primary: true,
        },
        {
          title: "Meter",
          units: [|{prefix: Unit, unit: Meter, power: 1}|],
          primary: true,
        },
        {
          title: "Kilometer",
          units: [|{prefix: Kilo, unit: Meter, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Inch",
          units: [|{prefix: Unit, unit: Inch, power: 1}|],
          primary: true,
        },
        {
          title: "Foot",
          units: [|{prefix: Unit, unit: Foot, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Yard",
          units: [|{prefix: Unit, unit: Yard, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Mile",
          units: [|{prefix: Unit, unit: Mile, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Nautical Mile",
          units: [|{prefix: Unit, unit: NauticalMile, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Light Year",
          units: [|{prefix: Unit, unit: LightYear, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Parsec",
          units: [|{prefix: Unit, unit: Parsec, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Angstrom",
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
        {
          title: "Femtogram",
          units: [|{prefix: Femto, unit: Gram, power: 1}|],
          primary: false,
        },
        {
          title: "Picogram",
          units: [|{prefix: Pico, unit: Gram, power: 1}|],
          primary: false,
        },
        {
          title: "Nanogram",
          units: [|{prefix: Nano, unit: Gram, power: 1}|],
          primary: false,
        },
        {
          title: "Microgram",
          units: [|{prefix: Micro, unit: Gram, power: 1}|],
          primary: false,
        },
        {
          title: "Milligram",
          units: [|{prefix: Milli, unit: Gram, power: 1}|],
          primary: false,
        },
        {
          title: "Gram",
          units: [|{prefix: Unit, unit: Gram, power: 1}|],
          primary: true,
        },
        {
          title: "Kilogram",
          units: [|{prefix: Kilo, unit: Gram, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Tonne",
          units: [|{prefix: Unit, unit: Tonne, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Ounce",
          units: [|{prefix: Unit, unit: Ounce, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Pound",
          units: [|{prefix: Unit, unit: Pound, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Stone",
          units: [|{prefix: Unit, unit: Stone, power: 1}|],
          primary: true,
        },
      |],
    |],
  },
  {
    title: "Area",
    data: [|
      [|
        {
          title: "Square Centimeters",
          units: [|{prefix: Centi, unit: Meter, power: 2}|],
          primary: true,
        },
        {
          title: "Square Meters",
          units: [|{prefix: Unit, unit: Meter, power: 2}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Square Inches",
          units: [|{prefix: Unit, unit: Inch, power: 2}|],
          primary: true,
        },
        {
          title: "Square Feet",
          units: [|{prefix: Unit, unit: Foot, power: 2}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Acre",
          units: [|{prefix: Unit, unit: Acre, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Hectare",
          units: [|{prefix: Unit, unit: Hectare, power: 1}|],
          primary: true,
        },
      |],
    |],
  },
  {
    title: "Volume",
    data: [|
      [|
        {
          title: "Cubic Centimeters",
          units: [|{prefix: Centi, unit: Meter, power: 3}|],
          primary: true,
        },
        {
          title: "Cubic Meters",
          units: [|{prefix: Unit, unit: Meter, power: 3}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Cubic Inches",
          units: [|{prefix: Unit, unit: Inch, power: 3}|],
          primary: true,
        },
        {
          title: "Cubic Feet",
          units: [|{prefix: Unit, unit: Foot, power: 3}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Milliliter",
          units: [|{prefix: Milli, unit: Liter, power: 1}|],
          primary: true,
        },
        {
          title: "Centiliter",
          units: [|{prefix: Centi, unit: Liter, power: 1}|],
          primary: true,
        },
        {
          title: "Liter",
          units: [|{prefix: Unit, unit: Liter, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Gallon",
          units: [|{prefix: Unit, unit: Gallon, power: 1}|],
          primary: true,
        },
        {
          title: "US Gallon",
          units: [|{prefix: Unit, unit: USGallon, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Quart",
          units: [|{prefix: Unit, unit: Quart, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Cup",
          units: [|{prefix: Unit, unit: Cup, power: 1}|],
          primary: true,
        },
        {
          title: "US Cup",
          units: [|{prefix: Unit, unit: USCup, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Teaspoon",
          units: [|{prefix: Unit, unit: Teaspoon, power: 1}|],
          primary: true,
        },
        {
          title: "Tablespoon",
          units: [|{prefix: Unit, unit: Tablespoon, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Fluid Ounce",
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
          title: "Miles per Hour",
          units: [|
            {prefix: Unit, unit: Mile, power: 1},
            {prefix: Unit, unit: Hour, power: (-1)},
          |],
          primary: true,
        },
        {
          title: "Kilometers per Hour",
          units: [|
            {prefix: Kilo, unit: Meter, power: 1},
            {prefix: Unit, unit: Hour, power: (-1)},
          |],
          primary: true,
        },
      |],
      [|
        {
          title: "Meters per Second",
          units: [|
            {prefix: Unit, unit: Meter, power: 1},
            {prefix: Unit, unit: Second, power: (-1)},
          |],
          primary: true,
        },
      |],
      [|
        {
          title: "Knot",
          units: [|{prefix: Unit, unit: Knot, power: 1}|],
          primary: true,
        },
      |],
    |],
  },
  {
    title: "Force",
    data: [|
      [|
        {
          title: "Newton",
          units: [|{prefix: Unit, unit: Newton, power: 1}|],
          primary: true,
        },
        {
          title: "FemtoNewton",
          units: [|{prefix: Femto, unit: Newton, power: 1}|],
          primary: false,
        },
        {
          title: "PicoNewton",
          units: [|{prefix: Pico, unit: Newton, power: 1}|],
          primary: false,
        },
        {
          title: "NanoNewton",
          units: [|{prefix: Nano, unit: Newton, power: 1}|],
          primary: false,
        },
        {
          title: "MicroNewton",
          units: [|{prefix: Micro, unit: Newton, power: 1}|],
          primary: false,
        },
        {
          title: "MilliNewton",
          units: [|{prefix: Milli, unit: Newton, power: 1}|],
          primary: false,
        },
        {
          title: "KiloNewton",
          units: [|{prefix: Kilo, unit: Newton, power: 1}|],
          primary: false,
        },
        {
          title: "MegaNewton",
          units: [|{prefix: Mega, unit: Newton, power: 1}|],
          primary: false,
        },
        {
          title: "GigaNewton",
          units: [|{prefix: Giga, unit: Newton, power: 1}|],
          primary: false,
        },
        {
          title: "TeraNewton",
          units: [|{prefix: Tera, unit: Newton, power: 1}|],
          primary: false,
        },
        {
          title: "PetaNewton",
          units: [|{prefix: Peta, unit: Newton, power: 1}|],
          primary: false,
        },
      |],
      [|
        {
          title: "Pound Force",
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
          title: "Atmosphere",
          units: [|{prefix: Unit, unit: Atmosphere, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Pascal",
          units: [|{prefix: Unit, unit: Pascal, power: 1}|],
          primary: true,
        },
        {
          title: "HectoPascal",
          units: [|{prefix: Hecto, unit: Pascal, power: 1}|],
          primary: false,
        },
        {
          title: "KiloPascal",
          units: [|{prefix: Kilo, unit: Pascal, power: 1}|],
          primary: false,
        },
      |],
      [|
        {
          title: "Bar",
          units: [|{prefix: Unit, unit: Bar, power: 1}|],
          primary: true,
        },
        {
          title: "Millibar",
          units: [|{prefix: Milli, unit: Bar, power: 1}|],
          primary: false,
        },
      |],
    |],
  },
  {
    title: "Energy",
    data: [|
      [|
        {
          title: "Joule",
          units: [|{prefix: Unit, unit: Joule, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Watt Hour",
          units: [|
            {prefix: Unit, unit: Watt, power: 1},
            {prefix: Unit, unit: Hour, power: 1},
          |],
          primary: true,
        },
        {
          title: "Kilowatt Hour",
          units: [|
            {prefix: Kilo, unit: Watt, power: 1},
            {prefix: Unit, unit: Hour, power: 1},
          |],
          primary: true,
        },
      |],
      [|
        {
          title: "Calorie",
          units: [|{prefix: Unit, unit: Calorie, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Electron Volt",
          units: [|{prefix: Unit, unit: ElectronVolt, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "BTU",
          units: [|{prefix: Unit, unit: BTU, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Therm",
          units: [|{prefix: Unit, unit: Therm, power: 1}|],
          primary: true,
        },
      |],
    |],
  },
  {
    title: "Power",
    data: [|
      [|
        {
          title: "Nanojoule",
          units: [|{prefix: Nano, unit: Joule, power: 1}|],
          primary: false,
        },
        {
          title: "Microjoule",
          units: [|{prefix: Micro, unit: Joule, power: 1}|],
          primary: false,
        },
        {
          title: "Millijoule",
          units: [|{prefix: Milli, unit: Joule, power: 1}|],
          primary: false,
        },
        {
          title: "Watt",
          units: [|{prefix: Unit, unit: Watt, power: 1}|],
          primary: true,
        },
        {
          title: "Kilojoule",
          units: [|{prefix: Kilo, unit: Joule, power: 1}|],
          primary: false,
        },
        {
          title: "Megajoule",
          units: [|{prefix: Mega, unit: Joule, power: 1}|],
          primary: false,
        },
        {
          title: "Gigajoule",
          units: [|{prefix: Giga, unit: Joule, power: 1}|],
          primary: false,
        },
      |],
    |],
  },
  {
    title: "Memory",
    data: [|
      [|
        {
          title: "Byte",
          units: [|{prefix: Unit, unit: Byte, power: 1}|],
          primary: true,
        },
        {
          title: "Kibibyte",
          units: [|{prefix: Kibi, unit: Byte, power: 1}|],
          primary: true,
        },
        {
          title: "Mebibyte",
          units: [|{prefix: Mebi, unit: Byte, power: 1}|],
          primary: true,
        },
        {
          title: "Gibibyte",
          units: [|{prefix: Gibi, unit: Byte, power: 1}|],
          primary: true,
        },
        {
          title: "Tebibyte",
          units: [|{prefix: Tebi, unit: Byte, power: 1}|],
          primary: false,
        },
        {
          title: "Pebibyte",
          units: [|{prefix: Pebi, unit: Byte, power: 1}|],
          primary: false,
        },
        {
          title: "Kilobyte",
          units: [|{prefix: Kilo, unit: Byte, power: 1}|],
          primary: false,
        },
        {
          title: "Megabyte",
          units: [|{prefix: Mega, unit: Byte, power: 1}|],
          primary: false,
        },
        {
          title: "Gigabyte",
          units: [|{prefix: Giga, unit: Byte, power: 1}|],
          primary: false,
        },
        {
          title: "Terabyte",
          units: [|{prefix: Tera, unit: Byte, power: 1}|],
          primary: false,
        },
        {
          title: "Petabyte",
          units: [|{prefix: Peta, unit: Byte, power: 1}|],
          primary: false,
        },
      |],
      [|
        {
          title: "Bit",
          units: [|{prefix: Unit, unit: Bit, power: 1}|],
          primary: true,
        },
        {
          title: "Kibibit",
          units: [|{prefix: Kibi, unit: Bit, power: 1}|],
          primary: true,
        },
        {
          title: "Mebibit",
          units: [|{prefix: Mebi, unit: Bit, power: 1}|],
          primary: true,
        },
        {
          title: "Gibibit",
          units: [|{prefix: Gibi, unit: Bit, power: 1}|],
          primary: true,
        },
        {
          title: "Tebibit",
          units: [|{prefix: Tebi, unit: Bit, power: 1}|],
          primary: false,
        },
        {
          title: "Pebibit",
          units: [|{prefix: Pebi, unit: Bit, power: 1}|],
          primary: false,
        },
        {
          title: "Kilobit",
          units: [|{prefix: Kilo, unit: Bit, power: 1}|],
          primary: false,
        },
        {
          title: "Megabit",
          units: [|{prefix: Mega, unit: Bit, power: 1}|],
          primary: false,
        },
        {
          title: "Gigabit",
          units: [|{prefix: Giga, unit: Bit, power: 1}|],
          primary: false,
        },
        {
          title: "Terabit",
          units: [|{prefix: Tera, unit: Bit, power: 1}|],
          primary: false,
        },
        {
          title: "Petabit",
          units: [|{prefix: Peta, unit: Bit, power: 1}|],
          primary: false,
        },
      |],
    |],
  },
  {
    title: "Temperature",
    data: [|
      [|
        {
          title: "Kelvin",
          units: [|{prefix: Unit, unit: Kelvin, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Celsius",
          units: [|{prefix: Unit, unit: Celsius, power: 1}|],
          primary: true,
        },
      |],
      [|
        {
          title: "Fahrenheit",
          units: [|{prefix: Unit, unit: Fahrenheit, power: 1}|],
          primary: true,
        },
      |],
    |],
  },
|];
