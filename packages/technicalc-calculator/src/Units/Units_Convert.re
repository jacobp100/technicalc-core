open Unit_Types;

let linearValue = (unit: unitType) =>
  switch (unit) {
  /* Time */
  | Second => 1.
  | Minute => 60.
  | Hour => 3600.
  | Day => 86400.
  | Week => 604800.
  | Month => 2628000.
  | Year => 31536000.
  | Decade => 315360000.
  | Century => 3155673600.
  | Femtosecond => 1e-15
  | Picosecond => 1e-12
  | Nanosecond => 1e-9
  | Microsecond => 1e-6
  | Millisecond => 1e-3
  /* Length */
  | Meter => 1.
  | Inch => 0.0254
  | Foot => 0.3048
  | Yard => 0.9144
  | Mile => 1609.
  | NauticalMile => 1852.
  | LightYear => 9.4605284e15
  | Parsec => 3.086e16
  | Angstrom => 1e-10
  | Femtometer => 1e-15
  | Picometer => 1e-12
  | Nanometer => 1e-9
  | Micrometer => 1e-6
  | Millimeter => 1e-3
  | Centimeter => 1e-2
  | Kilometer => 1e3
  /* Mass */
  | Gram => 1e-3
  | Tonne => 1e3
  | Ounce => 0.0283495
  | Pound => 0.453592
  | Stone => 6.35029
  | Femtogram => 1e-18
  | Picogram => 1e-15
  | Nanogram => 1e-12
  | Microgram => 1e-9
  | Milligram => 1e-6
  | Kilogram => 1.
  /* Area */
  | Acre => 4047.
  | Hectare => 1e4
  /* Volume */
  | Liter => 1e-3
  | Gallon => 4.54609e-3
  | USGallon => 3.785e-3
  | Quart => 9.464e-4
  | Cup => 2.4e-4
  | USCup => 2.3559e-4
  | Teaspoon => 4.929e-6
  | Tablespoon => 1.479e-5
  | FluidOunce => 2.8413e-5
  | Milliliter => 1e-6
  | Centiliter => 1e-5
  /* Speed */
  | Knot => 0.514444
  /* Force */
  | Newton => 1.
  | PoundForce => 4.448222
  | FemtoNewton => 1e-15
  | PicoNewton => 1e-12
  | NanoNewton => 1e-9
  | MicroNewton => 1e-6
  | MilliNewton => 1e-3
  | KiloNewton => 1e3
  | MegaNewton => 1e6
  | GigaNewton => 1e9
  | TeraNewton => 1e12
  | PetaNewton => 1e15
  /* Pressure */
  | Pascal => 1.
  | Atmosphere => 101325.
  | Bar => 100000.
  | HectoPascal => 100.
  | KiloPascal => 1000.
  | Millibar => 100.
  /* Energy */
  | Joule => 1.
  | Calorie => 4.184
  | ElectronVolt => 1.602e-19
  | BTU => 1055.
  | Therm => 1055000000.
  | Femtojoule => 1e-15
  | Picojoule => 1e-12
  | Nanojoule => 1e-9
  | Microjoule => 1e-6
  | Millijoule => 1e-3
  | Centijoule => 1e-2
  | Kilojoule => 1e3
  | Megajoule => 1e6
  | Gigajoule => 1e9
  | Terajoule => 1e12
  | Petajoule => 1e15
  /* Power */
  | Watt => 1.
  | Horsepower => 745.7
  | MetricHorsepower => 735.5
  | Nanowatt => 1e-9
  | Microwatt => 1e-6
  | Milliwatt => 1e-3
  | Kilowatt => 1e3
  | Megawatt => 1e6
  | Gigawatt => 1e9
  /* Memory */
  | Bit => 1.
  | Byte => 8.
  | Kilobit => 1e3
  | Megabit => 1e6
  | Gigabit => 1e9
  | Terabit => 1e12
  | Petabit => 1e15
  | Kibibit => 1024.
  | Mebibit => 1048576.
  | Gibibit => 1073741824.
  | Tebibit => 1099511627776.
  | Pebibit => 1125899906842624.
  | Kilobyte => 8e3
  | Megabyte => 8e6
  | Gigabyte => 8e9
  | Terabyte => 8e12
  | Petabyte => 8e15
  | Kibibyte => 8192.
  | Mebibyte => 8388608.
  | Gibibyte => 8589934592.
  | Tebibyte => 8796093022208.
  | Pebibyte => 9007199254740992.
  /* Temperature */
  | Kelvin => 1.
  | Celsius
  | Fahrenheit => assert(false)
  };

let celsiusToKelvin = value => Decimal.(value + ofFloat(273.15));
let fahrenheitToKelvin = value =>
  Decimal.((value - ofFloat(32.)) / ofFloat(1.8) + ofFloat(273.15));
let celsiusFromKelvin = value => Decimal.(value - ofFloat(273.15));
let fahrenheitFromKelvin = value =>
  Decimal.((value - ofFloat(273.15)) * ofFloat(1.8) + ofFloat(32.));

let%private transformUnits =
            (
              ~transformCelsius,
              ~transformFahrenheit,
              ~unitPowerMultiplier,
              value: Decimal.t,
              units: array(unitPower),
            ) => {
  let handleLinearUnit = (value, unit) =>
    switch (unit) {
    | (Celsius | Fahrenheit, _) => Decimal.nan
    | (linearUnit, power) =>
      let nextPower = power * unitPowerMultiplier;
      Decimal.(
        value * linearValue(linearUnit)->ofFloat ** Decimal.ofInt(nextPower)
      );
    };

  switch (units) {
  | [|(Celsius, 1)|] => transformCelsius(value)
  | [|(Fahrenheit, 1)|] => transformFahrenheit(value)
  | _ => Belt.Array.reduce(units, value, handleLinearUnit)
  };
};

let fromSi = (value: Value.t, units) =>
  transformUnits(
    ~transformCelsius=celsiusFromKelvin,
    ~transformFahrenheit=fahrenheitFromKelvin,
    ~unitPowerMultiplier=-1,
    Value.toDecimal(value),
    units,
  )
  ->Value.ofDecimal;

let toSi = (value: Value.t, units) =>
  transformUnits(
    ~transformCelsius=celsiusToKelvin,
    ~transformFahrenheit=fahrenheitToKelvin,
    ~unitPowerMultiplier=1,
    Value.toDecimal(value),
    units,
  )
  ->Value.ofDecimal;

let convert = (value: Value.t, ~fromUnits, ~toUnits) =>
  if (Unit_Dimensions.unitsCompatible(fromUnits, toUnits)) {
    value->toSi(fromUnits)->fromSi(toUnits);
  } else {
    `N;
  };
