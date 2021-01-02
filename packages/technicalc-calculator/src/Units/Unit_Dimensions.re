open Unit_Types;

type t = {
  length: int,
  time: int,
  mass: int,
  memory: int,
  temperature: int,
};

let%private empty = {length: 0, time: 0, mass: 0, memory: 0, temperature: 0};

let%private time = {...empty, time: 1};
let%private length = {...empty, length: 1};
let%private mass = {...empty, mass: 1};
let%private area = {...empty, length: 2};
let%private volume = {...empty, length: 3};
let%private speed = {...empty, length: 1, time: (-1)};
let%private force = {...empty, mass: 1, length: 1, time: (-2)};
let%private energy = {...empty, mass: 1, length: 2, time: (-2)};
let%private power = {...empty, mass: 1, length: 2, time: (-3)};
let%private pressure = {...empty, mass: 1, length: (-1), time: (-2)};
let%private memory = {...empty, memory: 1};
let%private temperature = {...empty, temperature: 1};

let ofUnit = (v: unitType) =>
  switch (v) {
  /* Time */
  | Second
  | Minute
  | Hour
  | Day
  | Week
  | Month
  | Year
  | Decade
  | Century => time
  /* Length */
  | Meter
  | Inch
  | Foot
  | Yard
  | Mile
  | NauticalMile
  | LightYear
  | Parsec
  | Angstrom => length
  /* Mass */
  | Gram
  | Tonne
  | Ounce
  | Pound
  | Stone => mass
  /* Area */
  | Acre
  | Hectare => area
  /* Volume */
  | Liter
  | Gallon
  | USGallon
  | Quart
  | Cup
  | USCup
  | Teaspoon
  | Tablespoon
  | FluidOunce => volume
  /* Speed */
  | Knot => speed
  /* Force */
  | Newton
  | PoundForce => force
  /* Pressure */
  | Pascal
  | Atmosphere
  | Bar => pressure
  /* Energy */
  | Joule
  | Calorie
  | ElectronVolt
  | BTU
  | Therm => energy
  /* Power */
  | Watt
  | Horsepower
  | MetricHorsepower => power
  /* Memory */
  | Bit
  | Byte => memory
  /* Temperature */
  | Kelvin
  | Celsius
  | Fahrenheit => temperature
  };

let ofUnitParts = (units: array(unitPart)) =>
  Belt.Array.reduceU(
    units,
    empty,
    (. comdinedDimensions, {unit, power}) => {
      let dimensions = ofUnit(unit);
      {
        length: comdinedDimensions.length + dimensions.length * power,
        time: comdinedDimensions.time + dimensions.time * power,
        mass: comdinedDimensions.mass + dimensions.mass * power,
        memory: comdinedDimensions.memory + dimensions.memory * power,
        temperature:
          comdinedDimensions.temperature + dimensions.temperature * power,
      };
    },
  );

let equal = (a: t, b: t) =>
  a.length == b.length
  && a.time == b.time
  && a.mass == b.mass
  && a.memory == b.memory
  && a.temperature == b.temperature;

let unitsCompatible = (a: array(unitPart), b: array(unitPart)) =>
  equal(ofUnitParts(a), ofUnitParts(b));
