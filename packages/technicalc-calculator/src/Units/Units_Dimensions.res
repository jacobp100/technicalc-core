open Units_Types

type dimensions = {
  length: int,
  time: int,
  mass: int,
  ampere: int,
  substance: int,
  memory: int,
  temperature: int,
}

%%private(
  let empty = {length: 0, time: 0, mass: 0, ampere: 0, substance: 0, memory: 0, temperature: 0}
)

%%private(let time = {...empty, time: 1})
%%private(let length = {...empty, length: 1})
%%private(let mass = {...empty, mass: 1})
%%private(let area = {...empty, length: 2})
%%private(let volume = {...empty, length: 3})
%%private(let speed = {...empty, length: 1, time: -1})
%%private(let force = {...empty, mass: 1, length: 1, time: -2})
%%private(let energy = {...empty, mass: 1, length: 2, time: -2})
%%private(let power = {...empty, mass: 1, length: 2, time: -3})
%%private(let volts = {...empty, mass: 1, length: 2, time: -3, ampere: -1})
%%private(let ampere = {...empty, ampere: 1})
%%private(let resistance = {...empty, mass: 1, length: 2, time: -3, ampere: -2})
%%private(let charge = {...empty, ampere: 1, time: 1})
%%private(let capacitance = {...empty, mass: -1, length: -2, time: 4, ampere: 2})
%%private(let flux = {...empty, mass: 1, length: 2, time: -2, ampere: -1})
%%private(let fluxDensity = {...empty, mass: 1, time: -2, ampere: -1})
%%private(let inductance = {...empty, mass: 1, length: 2, time: -2, ampere: -2})
%%private(let connductance = {...empty, mass: -1, length: -2, time: 3, ampere: 2})
%%private(let frequency = {...empty, time: -1})
%%private(let pressure = {...empty, mass: 1, length: -1, time: -2})
%%private(let substance = {...empty, substance: 1})
%%private(let memory = {...empty, memory: 1})
%%private(let temperature = {...empty, temperature: 1})

let ofUnit = (v: name) =>
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
  | ScandinavianMile
  | NauticalMile
  | LightYear
  | Parsec
  | Angstrom
  | Furlong
  | Chain
  | Link
  | Rod => length
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
  /* Voltage */
  | Volt => volts
  /* Ampere */
  | Ampere => ampere
  /* Resistance */
  | Ohm => resistance
  /* Charge */
  | Coulomb => charge
  /* Capacitance */
  | Farad => capacitance
  /* Flux */
  | Weber => flux
  /* Flux density */
  | Tesla => fluxDensity
  /* Inductance */
  | Henry => inductance
  /* Connductance */
  | Siemens => connductance
  /* Substance */
  | Mole => substance
  /* Frequency */
  | Hertz => frequency
  /* Memory */
  | Bit
  | Byte => memory
  /* Temperature */
  | Kelvin
  | Celsius
  | Fahrenheit => temperature
  }

let ofUnit = ({name, power}: t) => {
  let dimensions = ofUnit(name)
  {
    length: dimensions.length * power,
    time: dimensions.time * power,
    mass: dimensions.mass * power,
    ampere: dimensions.ampere * power,
    substance: dimensions.substance * power,
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
      ampere: comdinedDimensions.ampere + dimensions.ampere,
      substance: comdinedDimensions.substance + dimensions.substance,
      memory: comdinedDimensions.memory + dimensions.memory,
      temperature: comdinedDimensions.temperature + dimensions.temperature,
    }
  })

let size = (a: dimensions) =>
  (a.length !== 0 ? 1 : 0) +
  (a.time !== 0 ? 1 : 0) +
  (a.mass !== 0 ? 1 : 0) +
  (a.ampere !== 0 ? 1 : 0) +
  (a.substance !== 0 ? 1 : 0) +
  (a.memory !== 0 ? 1 : 0) + (a.temperature !== 0 ? 1 : 0)

let eq = (a: dimensions, b: dimensions) =>
  a.length == b.length &&
  a.time == b.time &&
  a.mass == b.mass &&
  a.ampere == b.ampere &&
  a.memory == b.memory &&
  a.substance == b.substance &&
  a.temperature == b.temperature
