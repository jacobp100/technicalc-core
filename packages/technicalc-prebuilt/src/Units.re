[@warning "-30"];

type section = {
  title: string,
  data: array(array(unitRow)),
}
and unitRow = {
  title: string,
  units: array(TechniCalcCalculator.Unit_Types.unitPower),
  primary: bool,
};

let units = [|
  {
    title: "Time",
    data: [|
      [|
        {title: "Millisecond", units: [|(Millisecond, 1)|], primary: true},
        {title: "Femtosecond", units: [|(Femtosecond, 1)|], primary: false},
        {title: "Picosecond", units: [|(Picosecond, 1)|], primary: false},
        {title: "Nanosecond", units: [|(Nanosecond, 1)|], primary: false},
        {title: "Microsecond", units: [|(Microsecond, 1)|], primary: false},
        {title: "Second", units: [|(Second, 1)|], primary: true},
      |],
      [|{title: "Minute", units: [|(Minute, 1)|], primary: true}|],
      [|{title: "Hour", units: [|(Hour, 1)|], primary: true}|],
      [|{title: "Day", units: [|(Day, 1)|], primary: true}|],
      [|{title: "Week", units: [|(Week, 1)|], primary: true}|],
      [|{title: "Month", units: [|(Month, 1)|], primary: true}|],
      [|{title: "Year", units: [|(Year, 1)|], primary: true}|],
      [|{title: "Decade", units: [|(Decade, 1)|], primary: true}|],
      [|{title: "Century", units: [|(Century, 1)|], primary: true}|],
    |],
  },
  {
    title: "Length",
    data: [|
      [|
        {title: "Femtometer", units: [|(Femtometer, 1)|], primary: false},
        {title: "Picometer", units: [|(Picometer, 1)|], primary: false},
        {title: "Nanometer", units: [|(Nanometer, 1)|], primary: false},
        {title: "Micrometer", units: [|(Micrometer, 1)|], primary: false},
        {title: "Millimeter", units: [|(Millimeter, 1)|], primary: true},
        {title: "Centimeter", units: [|(Centimeter, 1)|], primary: true},
        {title: "Meter", units: [|(Meter, 1)|], primary: true},
        {title: "Kilometer", units: [|(Kilometer, 1)|], primary: true},
      |],
      [|
        {title: "Inch", units: [|(Inch, 1)|], primary: true},
        {title: "Foot", units: [|(Foot, 1)|], primary: true},
      |],
      [|{title: "Yard", units: [|(Yard, 1)|], primary: true}|],
      [|{title: "Mile", units: [|(Mile, 1)|], primary: true}|],
      [|
        {
          title: "Nautical Mile",
          units: [|(NauticalMile, 1)|],
          primary: true,
        },
      |],
      [|{title: "Light Year", units: [|(LightYear, 1)|], primary: true}|],
      [|{title: "Parsec", units: [|(Parsec, 1)|], primary: true}|],
      [|{title: "Angstrom", units: [|(Angstrom, 1)|], primary: true}|],
    |],
  },
  {
    title: "Mass",
    data: [|
      [|
        {title: "Femtogram", units: [|(Femtogram, 1)|], primary: false},
        {title: "Picogram", units: [|(Picogram, 1)|], primary: false},
        {title: "Nanogram", units: [|(Nanogram, 1)|], primary: false},
        {title: "Microgram", units: [|(Microgram, 1)|], primary: false},
        {title: "Milligram", units: [|(Milligram, 1)|], primary: false},
        {title: "Gram", units: [|(Gram, 1)|], primary: true},
        {title: "Kilogram", units: [|(Kilogram, 1)|], primary: true},
      |],
      [|{title: "Tonne", units: [|(Tonne, 1)|], primary: true}|],
      [|{title: "Ounce", units: [|(Ounce, 1)|], primary: true}|],
      [|{title: "Pound", units: [|(Pound, 1)|], primary: true}|],
      [|{title: "Stone", units: [|(Stone, 1)|], primary: true}|],
    |],
  },
  {
    title: "Area",
    data: [|
      [|
        {
          title: "Square Centimeters",
          units: [|(Centimeter, 2)|],
          primary: true,
        },
        {title: "Square Meters", units: [|(Meter, 2)|], primary: true},
      |],
      [|
        {title: "Square Inches", units: [|(Inch, 2)|], primary: true},
        {title: "Square Feet", units: [|(Foot, 2)|], primary: true},
      |],
      [|{title: "Acre", units: [|(Acre, 1)|], primary: true}|],
      [|{title: "Hectare", units: [|(Hectare, 1)|], primary: true}|],
    |],
  },
  {
    title: "Volume",
    data: [|
      [|
        {
          title: "Cubic Centimeters",
          units: [|(Centimeter, 3)|],
          primary: true,
        },
        {title: "Cubic Meters", units: [|(Meter, 3)|], primary: true},
      |],
      [|
        {title: "Cubic Inches", units: [|(Inch, 3)|], primary: true},
        {title: "Cubic Feet", units: [|(Foot, 3)|], primary: true},
      |],
      [|
        {title: "Milliliter", units: [|(Milliliter, 1)|], primary: true},
        {title: "Centiliter", units: [|(Centiliter, 1)|], primary: true},
        {title: "Liter", units: [|(Liter, 1)|], primary: true},
      |],
      [|
        {title: "Gallon", units: [|(Gallon, 1)|], primary: true},
        {title: "US Gallon", units: [|(USGallon, 1)|], primary: true},
      |],
      [|{title: "Quart", units: [|(Quart, 1)|], primary: true}|],
      [|
        {title: "Cup", units: [|(Cup, 1)|], primary: true},
        {title: "US Cup", units: [|(USCup, 1)|], primary: true},
      |],
      [|
        {title: "Teaspoon", units: [|(Teaspoon, 1)|], primary: true},
        {title: "Tablespoon", units: [|(Tablespoon, 1)|], primary: true},
      |],
      [|
        {title: "Fluid Ounce", units: [|(FluidOunce, 1)|], primary: true},
      |],
    |],
  },
  {
    title: "Speed",
    data: [|
      [|
        {
          title: "Miles per Hour",
          units: [|(Mile, 1), (Hour, (-1))|],
          primary: true,
        },
        {
          title: "Kilometers per Hour",
          units: [|(Kilometer, 1), (Hour, (-1))|],
          primary: true,
        },
      |],
      [|
        {
          title: "Meters per Second",
          units: [|(Meter, 1), (Second, (-1))|],
          primary: true,
        },
      |],
      [|{title: "Knot", units: [|(Knot, 1)|], primary: true}|],
    |],
  },
  {
    title: "Force",
    data: [|
      [|
        {title: "Newton", units: [|(Newton, 1)|], primary: true},
        {title: "FemtoNewton", units: [|(FemtoNewton, 1)|], primary: false},
        {title: "PicoNewton", units: [|(PicoNewton, 1)|], primary: false},
        {title: "NanoNewton", units: [|(NanoNewton, 1)|], primary: false},
        {title: "MicroNewton", units: [|(MicroNewton, 1)|], primary: false},
        {title: "MilliNewton", units: [|(MilliNewton, 1)|], primary: false},
        {title: "KiloNewton", units: [|(KiloNewton, 1)|], primary: false},
        {title: "MegaNewton", units: [|(MegaNewton, 1)|], primary: false},
        {title: "GigaNewton", units: [|(GigaNewton, 1)|], primary: false},
        {title: "TeraNewton", units: [|(TeraNewton, 1)|], primary: false},
        {title: "PetaNewton", units: [|(PetaNewton, 1)|], primary: false},
      |],
      [|
        {title: "Pound Force", units: [|(PoundForce, 1)|], primary: true},
      |],
    |],
  },
  {
    title: "Pressure",
    data: [|
      [|{title: "Atmosphere", units: [|(Atmosphere, 1)|], primary: true}|],
      [|
        {title: "Pascal", units: [|(Pascal, 1)|], primary: true},
        {title: "HectoPascal", units: [|(HectoPascal, 1)|], primary: false},
        {title: "KiloPascal", units: [|(KiloPascal, 1)|], primary: false},
      |],
      [|
        {title: "Bar", units: [|(Bar, 1)|], primary: true},
        {title: "Millibar", units: [|(Millibar, 1)|], primary: false},
      |],
    |],
  },
  {
    title: "Energy",
    data: [|
      [|{title: "Joule", units: [|(Joule, 1)|], primary: true}|],
      [|
        {
          title: "Watt Hour",
          units: [|(Watt, 1), (Hour, 1)|],
          primary: true,
        },
        {
          title: "Kilowatt Hour",
          units: [|(Kilowatt, 1), (Hour, 1)|],
          primary: true,
        },
      |],
      [|{title: "Calorie", units: [|(Calorie, 1)|], primary: true}|],
      [|
        {
          title: "Electron Volt",
          units: [|(ElectronVolt, 1)|],
          primary: true,
        },
      |],
      [|{title: "BTU", units: [|(BTU, 1)|], primary: true}|],
      [|{title: "Therm", units: [|(Therm, 1)|], primary: true}|],
    |],
  },
  {
    title: "Power",
    data: [|
      [|
        {title: "Nanojoule", units: [|(Nanojoule, 1)|], primary: false},
        {title: "Microjoule", units: [|(Microjoule, 1)|], primary: false},
        {title: "Millijoule", units: [|(Millijoule, 1)|], primary: false},
        {title: "Watt", units: [|(Watt, 1)|], primary: true},
        {title: "Kilojoule", units: [|(Kilojoule, 1)|], primary: false},
        {title: "Megajoule", units: [|(Megajoule, 1)|], primary: false},
        {title: "Gigajoule", units: [|(Gigajoule, 1)|], primary: false},
      |],
    |],
  },
  {
    title: "Memory",
    data: [|
      [|
        {title: "Byte", units: [|(Byte, 1)|], primary: true},
        {title: "Kibibyte", units: [|(Kibibyte, 1)|], primary: true},
        {title: "Mebibyte", units: [|(Mebibyte, 1)|], primary: true},
        {title: "Gibibyte", units: [|(Gibibyte, 1)|], primary: true},
        {title: "Tebibyte", units: [|(Tebibyte, 1)|], primary: false},
        {title: "Pebibyte", units: [|(Pebibyte, 1)|], primary: false},
        {title: "Kilobyte", units: [|(Kilobyte, 1)|], primary: false},
        {title: "Megabyte", units: [|(Megabyte, 1)|], primary: false},
        {title: "Gigabyte", units: [|(Gigabyte, 1)|], primary: false},
        {title: "Terabyte", units: [|(Terabyte, 1)|], primary: false},
        {title: "Petabyte", units: [|(Petabyte, 1)|], primary: false},
      |],
      [|
        {title: "Bit", units: [|(Bit, 1)|], primary: true},
        {title: "Kibibit", units: [|(Kibibit, 1)|], primary: true},
        {title: "Mebibit", units: [|(Mebibit, 1)|], primary: true},
        {title: "Gibibit", units: [|(Gibibit, 1)|], primary: true},
        {title: "Tebibit", units: [|(Tebibit, 1)|], primary: false},
        {title: "Pebibit", units: [|(Pebibit, 1)|], primary: false},
        {title: "Kilobit", units: [|(Kilobit, 1)|], primary: false},
        {title: "Megabit", units: [|(Megabit, 1)|], primary: false},
        {title: "Gigabit", units: [|(Gigabit, 1)|], primary: false},
        {title: "Terabit", units: [|(Terabit, 1)|], primary: false},
        {title: "Petabit", units: [|(Petabit, 1)|], primary: false},
      |],
    |],
  },
  {
    title: "Temperature",
    data: [|
      [|{title: "Kelvin", units: [|(Kelvin, 1)|], primary: true}|],
      [|{title: "Celsius", units: [|(Celsius, 1)|], primary: true}|],
      [|{title: "Fahrenheit", units: [|(Fahrenheit, 1)|], primary: true}|],
    |],
  },
|];
