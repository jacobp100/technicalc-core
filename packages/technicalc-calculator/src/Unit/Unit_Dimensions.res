open Unit_Types

type dimensions = {
  length: int,
  time: int,
  mass: int,
  memory: int,
  temperature: int,
}

let emptyDimensions = {length: 0, time: 0, mass: 0, memory: 0, temperature: 0}

%%private(let time = {...emptyDimensions, time: 1})
%%private(let length = {...emptyDimensions, length: 1})
%%private(let mass = {...emptyDimensions, mass: 1})
%%private(let area = {...emptyDimensions, length: 2})
%%private(let volume = {...emptyDimensions, length: 3})
%%private(let speed = {...emptyDimensions, length: 1, time: -1})
%%private(let force = {...emptyDimensions, mass: 1, length: 1, time: -2})
%%private(let energy = {...emptyDimensions, mass: 1, length: 2, time: -2})
%%private(let power = {...emptyDimensions, mass: 1, length: 2, time: -3})
%%private(let pressure = {...emptyDimensions, mass: 1, length: -1, time: -2})
%%private(let memory = {...emptyDimensions, memory: 1})
%%private(let temperature = {...emptyDimensions, temperature: 1})

%%private(
  let ofName = (x: name) =>
    switch x {
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
    | ScandinavianMile
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
)

let dimensions = ({name, power}: t) => {
  let dimensions = ofName(name)
  {
    length: dimensions.length * power,
    time: dimensions.time * power,
    mass: dimensions.mass * power,
    memory: dimensions.memory * power,
    temperature: dimensions.temperature * power,
  }
}

let dimensionsSize = (a: dimensions) =>
  (a.length !== 0 ? 1 : 0) +
  (a.time !== 0 ? 1 : 0) +
  (a.mass !== 0 ? 1 : 0) +
  (a.memory !== 0 ? 1 : 0) + (a.temperature !== 0 ? 1 : 0)

let dimensionsEq = (a: dimensions, b: dimensions) =>
  a.length == b.length &&
    (a.time == b.time &&
    (a.mass == b.mass && (a.memory == b.memory && a.temperature == b.temperature)))

let dimensionsAdd = (a, b) => {
  length: a.length + b.length,
  time: a.time + b.time,
  mass: a.mass + b.mass,
  memory: a.memory + b.memory,
  temperature: a.temperature + b.temperature,
}
