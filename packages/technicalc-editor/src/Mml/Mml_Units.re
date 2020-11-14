let unitMmlSymbol = (unit: TechniCalcCalculator.Unit_Types.unitType) =>
  switch (unit) {
  /* Time */
  | Second => "s"
  | Minute => "min"
  | Hour => "h"
  | Day => "d"
  | Week => "week"
  | Month => "month"
  | Year => "year"
  | Decade => "decade"
  | Century => "century"
  | Femtosecond => "fs"
  | Picosecond => "ps"
  | Nanosecond => "ns"
  | Microsecond => "&#x3BC;s"
  | Millisecond => "ms"
  /* Length */
  | Meter => "m"
  | Inch => "in"
  | Foot => "ft"
  | Yard => "yd"
  | Mile => "mi"
  | NauticalMile => "NM"
  | LightYear => "ly"
  | Parsec => "pc"
  | Angstrom => "&#x212B;"
  | Femtometer => "fm"
  | Picometer => "pm"
  | Nanometer => "nm"
  | Micrometer => "&#x3BC;m"
  | Millimeter => "mm"
  | Centimeter => "cm"
  | Kilometer => "km"
  /* Mass */
  | Gram => "g"
  | Tonne => "T"
  | Ounce => "oz"
  | Pound => "lb"
  | Stone => "st"
  | Femtogram => "fg"
  | Picogram => "pg"
  | Nanogram => "ng"
  | Microgram => "&#x3BC;g"
  | Milligram => "mg"
  | Kilogram => "kg"
  /* Area */
  | Acre => "acre"
  | Hectare => "ha"
  /* Volume */
  | Liter => "l"
  | Gallon => "Gal"
  | USGallon => "US Gal"
  | Quart => "qt"
  | Cup => "cup"
  | USCup => "US cup"
  | Teaspoon => "tsp"
  | Tablespoon => "tbsp"
  | FluidOunce => "fl oz"
  | Milliliter => "ml"
  | Centiliter => "cl"
  /* Speed */
  | Knot => "kn"
  /* Force */
  | Newton => "N"
  | PoundForce => "lbf"
  | FemtoNewton => "fN"
  | PicoNewton => "pN"
  | NanoNewton => "nN"
  | MicroNewton => "&#x3BC;N"
  | MilliNewton => "mN"
  | KiloNewton => "kN"
  | MegaNewton => "MN"
  | GigaNewton => "GN"
  | TeraNewton => "TN"
  | PetaNewton => "PN"
  /* Pressure */
  | Pascal => "Pa"
  | Atmosphere => "atm"
  | Bar => "bar"
  | HectoPascal => "hPa"
  | KiloPascal => "kPa"
  | Millibar => "mbar"
  /* Energy */
  | Joule => "J"
  | Calorie => "cal"
  | ElectronVolt => "eV"
  | BTU => "Btu"
  | Therm => "thm"
  | Femtojoule => "fJ"
  | Picojoule => "pJ"
  | Nanojoule => "nJ"
  | Microjoule => "&#x3BC;J"
  | Millijoule => "mJ"
  | Centijoule => "J"
  | Kilojoule => "kJ"
  | Megajoule => "MJ"
  | Gigajoule => "GJ"
  | Terajoule => "TJ"
  | Petajoule => "PJ"
  /* Power */
  | Watt => "W"
  | Horsepower => "hp"
  | MetricHorsepower => "PS"
  | Nanowatt => "nW"
  | Microwatt => "&#x3BC;W"
  | Milliwatt => "mW"
  | Kilowatt => "kW"
  | Megawatt => "MW"
  | Gigawatt => "GW"
  /* Memory */
  | Bit => "b"
  | Byte => "B"
  | Kilobit => "kb"
  | Megabit => "Mb"
  | Gigabit => "Gb"
  | Terabit => "Tb"
  | Petabit => "Pb"
  | Kibibit => "Kib"
  | Mebibit => "Mib"
  | Gibibit => "Gib"
  | Tebibit => "Tib"
  | Pebibit => "Pib"
  | Kilobyte => "kB"
  | Megabyte => "MB"
  | Gigabyte => "GB"
  | Terabyte => "TB"
  | Petabyte => "PB"
  | Kibibyte => "KiB"
  | Mebibyte => "MiB"
  | Gibibyte => "GiB"
  | Tebibyte => "TiB"
  | Pebibyte => "PiB"
  /* Temperature */
  | Kelvin => "K"
  | Celsius => "&#x00B0;C"
  | Fahrenheit => "&#x00B0;F"
  };

let unitToMml = (unit: TechniCalcCalculator.Unit_Types.unitType) =>
  switch (unit) {
  /* Work around :( */
  | Angstrom => "<mi mathvariant=\"normal\">A</mi>"
  | _ => "<mi mathvariant=\"normal\">" ++ unitMmlSymbol(unit) ++ "</mi>"
  };

let unitPowerToMml =
  (. (unit, power): TechniCalcCalculator.Unit_Types.unitPower) =>
    switch (power) {
    | 1 => unitToMml(unit)
    | _ =>
      let powerMml = "<mn>" ++ string_of_int(power) ++ "</mn>";
      "<msup>" ++ unitToMml(unit) ++ powerMml ++ "</msup>";
    };

let unitPowersToMml =
    (units: array(TechniCalcCalculator.Unit_Types.unitPower)) => {
  let unitsMmlList =
    Belt.Array.mapU(units, unitPowerToMml)->Belt.List.fromArray;
  String.concat("<mspace width=\"0.1em\" />", unitsMmlList);
};
