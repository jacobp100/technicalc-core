open Unit_Types

type dimensions = {
  length: int,
  time: int,
  mass: int,
  memory: int,
  temperature: int,
}

%%private(let empty = {length: 0, time: 0, mass: 0, memory: 0, temperature: 0})

%%private(let time = {...empty, time: 1})
%%private(let length = {...empty, length: 1})
%%private(let mass = {...empty, mass: 1})
%%private(let area = {...empty, length: 2})
%%private(let volume = {...empty, length: 3})
%%private(let speed = {...empty, length: 1, time: -1})
%%private(let force = {...empty, mass: 1, length: 1, time: -2})
%%private(let energy = {...empty, mass: 1, length: 2, time: -2})
%%private(let power = {...empty, mass: 1, length: 2, time: -3})
%%private(let pressure = {...empty, mass: 1, length: -1, time: -2})
%%private(let memory = {...empty, memory: 1})
%%private(let temperature = {...empty, temperature: 1})

let ofUnit = (v: unitType) =>
  switch v {
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
  | USQuart
  | Cup
  | USCup
  | Pint
  | USPint
  | Teaspoon
  | USTeaspoon
  | Tablespoon
  | USTablespoon
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
  }

let ofUnit = ({type_, power}: t) => {
  let dimensions = ofUnit(type_)
  {
    length: dimensions.length * power,
    time: dimensions.time * power,
    mass: dimensions.mass * power,
    memory: dimensions.memory * power,
    temperature: dimensions.temperature * power,
  }
}

let ofUnits = (units: array<t>) =>
  Belt.Array.reduceU(units, empty, (. comdinedDimensions, t) => {
    let dimensions = ofUnit(t)
    {
      length: comdinedDimensions.length + dimensions.length,
      time: comdinedDimensions.time + dimensions.time,
      mass: comdinedDimensions.mass + dimensions.mass,
      memory: comdinedDimensions.memory + dimensions.memory,
      temperature: comdinedDimensions.temperature + dimensions.temperature,
    }
  })

let size = (a: dimensions) =>
  (a.length !== 0 ? 1 : 0) +
  (a.time !== 0 ? 1 : 0) +
  (a.mass !== 0 ? 1 : 0) +
  (a.memory !== 0 ? 1 : 0) + (a.temperature !== 0 ? 1 : 0)

let eq = (a: dimensions, b: dimensions) =>
  a.length == b.length &&
    (a.time == b.time &&
    (a.mass == b.mass && (a.memory == b.memory && a.temperature == b.temperature)))
