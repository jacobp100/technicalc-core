[@warning "-30"];

type section = {
  title: string,
  data: array(array(unitRow)),
}
and unitRow = {
  title: string,
  unit: TechniCalcCalculator.Unit_Types.unitType,
  primary: bool,
};

let units = [|
  {
    title: "Time",
    data: [|
      [|
        {title: "Millisecond", unit: Millisecond, primary: true},
        {title: "Femtosecond", unit: Femtosecond, primary: false},
        {title: "Picosecond", unit: Picosecond, primary: false},
        {title: "Nanosecond", unit: Nanosecond, primary: false},
        {title: "Microsecond", unit: Microsecond, primary: false},
        {title: "Second", unit: Second, primary: true},
      |],
      [|{title: "Minute", unit: Minute, primary: true}|],
      [|{title: "Hour", unit: Hour, primary: true}|],
      [|{title: "Day", unit: Day, primary: true}|],
      [|{title: "Week", unit: Week, primary: true}|],
      [|{title: "Month", unit: Month, primary: true}|],
      [|{title: "Year", unit: Year, primary: true}|],
      [|{title: "Decade", unit: Decade, primary: true}|],
      [|{title: "Century", unit: Century, primary: true}|],
    |],
  },
  {
    title: "Length",
    data: [|
      [|
        {title: "Femtometer", unit: Femtometer, primary: false},
        {title: "Picometer", unit: Picometer, primary: false},
        {title: "Nanometer", unit: Nanometer, primary: false},
        {title: "Micrometer", unit: Micrometer, primary: false},
        {title: "Millimeter", unit: Millimeter, primary: true},
        {title: "Centimeter", unit: Centimeter, primary: true},
        {title: "Meter", unit: Meter, primary: true},
        {title: "Kilometer", unit: Kilometer, primary: true},
      |],
      [|{title: "Inch", unit: Inch, primary: true}|],
      [|{title: "Foot", unit: Foot, primary: true}|],
      [|{title: "Yard", unit: Yard, primary: true}|],
      [|{title: "Mile", unit: Mile, primary: true}|],
      [|{title: "Nautical Mile", unit: NauticalMile, primary: true}|],
      [|{title: "Light Year", unit: LightYear, primary: true}|],
      [|{title: "Parsec", unit: Parsec, primary: true}|],
      [|{title: "Angstrom", unit: Angstrom, primary: true}|],
    |],
  },
  {
    title: "Mass",
    data: [|
      [|
        {title: "Femtogram", unit: Femtogram, primary: false},
        {title: "Picogram", unit: Picogram, primary: false},
        {title: "Nanogram", unit: Nanogram, primary: false},
        {title: "Microgram", unit: Microgram, primary: false},
        {title: "Milligram", unit: Milligram, primary: false},
        {title: "Gram", unit: Gram, primary: true},
        {title: "Kilogram", unit: Kilogram, primary: true},
      |],
      [|{title: "Tonne", unit: Tonne, primary: true}|],
      [|{title: "Ounce", unit: Ounce, primary: true}|],
      [|{title: "Pound", unit: Pound, primary: true}|],
      [|{title: "Stone", unit: Stone, primary: true}|],
    |],
  },
  {
    title: "Area",
    data: [|
      [|{title: "Acre", unit: Acre, primary: true}|],
      [|{title: "Hectare", unit: Hectare, primary: true}|],
    |],
  },
  {
    title: "Volume",
    data: [|
      [|
        {title: "Milliliter", unit: Milliliter, primary: true},
        {title: "Centiliter", unit: Centiliter, primary: true},
        {title: "Liter", unit: Liter, primary: true},
      |],
      [|
        {title: "Gallon", unit: Gallon, primary: true},
        {title: "US Gallon", unit: USGallon, primary: true},
      |],
      [|{title: "Quart", unit: Quart, primary: true}|],
      [|
        {title: "Cup", unit: Cup, primary: true},
        {title: "US Cup", unit: USCup, primary: true},
      |],
      [|
        {title: "Teaspoon", unit: Teaspoon, primary: true},
        {title: "Tablespoon", unit: Tablespoon, primary: true},
      |],
      [|{title: "Fluid Ounce", unit: FluidOunce, primary: true}|],
    |],
  },
  {
    title: "Speed",
    data: [|[|{title: "Knot", unit: Knot, primary: true}|]|],
  },
  {
    title: "Force",
    data: [|
      [|
        {title: "Newton", unit: Newton, primary: true},
        {title: "FemtoNewton", unit: FemtoNewton, primary: false},
        {title: "PicoNewton", unit: PicoNewton, primary: false},
        {title: "NanoNewton", unit: NanoNewton, primary: false},
        {title: "MicroNewton", unit: MicroNewton, primary: false},
        {title: "MilliNewton", unit: MilliNewton, primary: false},
        {title: "KiloNewton", unit: KiloNewton, primary: false},
        {title: "MegaNewton", unit: MegaNewton, primary: false},
        {title: "GigaNewton", unit: GigaNewton, primary: false},
        {title: "TeraNewton", unit: TeraNewton, primary: false},
        {title: "PetaNewton", unit: PetaNewton, primary: false},
      |],
      [|{title: "Pound Force", unit: PoundForce, primary: true}|],
    |],
  },
  {
    title: "Pressure",
    data: [|
      [|{title: "Atmosphere", unit: Atmosphere, primary: true}|],
      [|
        {title: "Pascal", unit: Pascal, primary: true},
        {title: "HectoPascal", unit: HectoPascal, primary: false},
        {title: "KiloPascal", unit: KiloPascal, primary: false},
      |],
      [|
        {title: "Bar", unit: Bar, primary: true},
        {title: "Millibar", unit: Millibar, primary: false},
      |],
    |],
  },
  {
    title: "Energy",
    data: [|
      [|{title: "Joule", unit: Joule, primary: true}|],
      [|{title: "Calorie", unit: Calorie, primary: true}|],
      [|{title: "Electron Volt", unit: ElectronVolt, primary: true}|],
      [|{title: "BTU", unit: BTU, primary: true}|],
      [|{title: "Therm", unit: Therm, primary: true}|],
    |],
  },
  {
    title: "Power",
    data: [|
      [|
        {title: "Nanojoule", unit: Nanojoule, primary: false},
        {title: "Microjoule", unit: Microjoule, primary: false},
        {title: "Millijoule", unit: Millijoule, primary: false},
        {title: "Watt", unit: Watt, primary: true},
        {title: "Kilojoule", unit: Kilojoule, primary: false},
        {title: "Megajoule", unit: Megajoule, primary: false},
        {title: "Gigajoule", unit: Gigajoule, primary: false},
      |],
    |],
  },
  {
    title: "Memory",
    data: [|
      [|
        {title: "Byte", unit: Byte, primary: true},
        {title: "Kibibyte", unit: Kibibyte, primary: true},
        {title: "Mebibyte", unit: Mebibyte, primary: true},
        {title: "Gibibyte", unit: Gibibyte, primary: true},
        {title: "Tebibyte", unit: Tebibyte, primary: false},
        {title: "Pebibyte", unit: Pebibyte, primary: false},
        {title: "Kilobyte", unit: Kilobyte, primary: false},
        {title: "Megabyte", unit: Megabyte, primary: false},
        {title: "Gigabyte", unit: Gigabyte, primary: false},
        {title: "Terabyte", unit: Terabyte, primary: false},
        {title: "Petabyte", unit: Petabyte, primary: false},
      |],
      [|
        {title: "Bit", unit: Bit, primary: true},
        {title: "Kibibit", unit: Kibibit, primary: true},
        {title: "Mebibit", unit: Mebibit, primary: true},
        {title: "Gibibit", unit: Gibibit, primary: true},
        {title: "Tebibit", unit: Tebibit, primary: false},
        {title: "Pebibit", unit: Pebibit, primary: false},
        {title: "Kilobit", unit: Kilobit, primary: false},
        {title: "Megabit", unit: Megabit, primary: false},
        {title: "Gigabit", unit: Gigabit, primary: false},
        {title: "Terabit", unit: Terabit, primary: false},
        {title: "Petabit", unit: Petabit, primary: false},
      |],
    |],
  },
  {
    title: "Temperature",
    data: [|
      [|{title: "Kelvin", unit: Kelvin, primary: true}|],
      [|{title: "Celsius", unit: Celsius, primary: true}|],
      [|{title: "Fahrenheit", unit: Fahrenheit, primary: true}|],
    |],
  },
|];
