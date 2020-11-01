/*
 This would be much better as a polymorphic variant, but it meant any switch
 statement over values took over 1kb of JS to compile
 */

type unitType =
  /* Time */
  | Second
  | Minute
  | Hour
  | Day
  | Week
  | Month
  | Year
  | Decade
  | Century
  | Femtosecond
  | Picosecond
  | Nanosecond
  | Microsecond
  | Millisecond
  /* Length */
  | Meter
  | Inch
  | Foot
  | Yard
  | Mile
  | NauticalMile
  | LightYear
  | Parsec
  | Angstrom
  | Femtometer
  | Picometer
  | Nanometer
  | Micrometer
  | Millimeter
  | Centimeter
  | Kilometer
  /* Mass */
  | Gram
  | Tonne
  | Ounce
  | Pound
  | Stone
  | Femtogram
  | Picogram
  | Nanogram
  | Microgram
  | Milligram
  | Kilogram
  /* Area */
  | Acre
  | Hectare
  /* Volume */
  | Liter
  | Gallon
  | USGallon
  | Quart
  | Cup
  | USCup
  | Teaspoon
  | Tablespoon
  | FluidOunce
  | Milliliter
  | Centiliter
  /* Speed */
  | Knot
  /* Force */
  | Newton
  | PoundForce
  /* Pressure */
  | Pascal
  | Atmosphere
  | Bar
  | HectoPascal
  | Millibar
  /* Energy */
  | Joule
  | Calorie
  | ElectronVolt
  | BTU
  | Therm
  | Femtojoule
  | Picojoule
  | Nanojoule
  | Microjoule
  | Millijoule
  | Centijoule
  | Kilojoule
  | Megajoule
  | Gigajoule
  | Terajoule
  | Petajoule
  /* Power */
  | Watt
  | Horsepower
  | MetricHorsepower
  | Nanowatt
  | Microwatt
  | Milliwatt
  | Kilowatt
  | Megawatt
  | Gigawatt
  /* Memory */
  | Bit
  | Byte
  | Kilobit
  | Megabit
  | Gigabit
  | Terabit
  | Petabit
  | Kibibit
  | Mebibit
  | Gibibit
  | Tebibit
  | Pebibit
  | Kilobyte
  | Megabyte
  | Gigabyte
  | Terabyte
  | Petabyte
  | Kibibyte
  | Mebibyte
  | Gibibyte
  | Tebibyte
  | Pebibyte
  /* Temperature */
  | Kelvin
  | Celsius
  | Fahrenheit;

type unitPower = (unitType, int);
