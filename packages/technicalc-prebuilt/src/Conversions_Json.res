type unitsRow = {
  composite: bool,
  units: array<TechniCalcCalculator.Units.t>,
}

type commonUnits = {
  dimensions: TechniCalcCalculator.Units_Dimensions.dimensions,
  units: array<unitsRow>,
}

let json: array<commonUnits> = [
  {
    dimensions: {
      length: 1,
      time: 0,
      mass: 0,
      ampere: 0,
      substance: 0,
      memory: 0,
      temperature: 0,
    },
    units: [
      {composite: false, units: [{prefix: Milli, name: Meter, power: 1}]},
      {composite: false, units: [{prefix: Centi, name: Meter, power: 1}]},
      {composite: false, units: [{prefix: Unit, name: Meter, power: 1}]},
      {composite: false, units: [{prefix: Unit, name: Inch, power: 1}]},
      {composite: false, units: [{prefix: Unit, name: Foot, power: 1}]},
      {
        composite: true,
        units: [{prefix: Unit, name: Foot, power: 1}, {prefix: Unit, name: Inch, power: 1}],
      },
    ],
  },
  {
    dimensions: {
      length: 2,
      time: 0,
      mass: 0,
      ampere: 0,
      substance: 0,
      memory: 0,
      temperature: 0,
    },
    units: [
      {composite: false, units: [{prefix: Milli, name: Meter, power: 2}]},
      {composite: false, units: [{prefix: Centi, name: Meter, power: 2}]},
      {composite: false, units: [{prefix: Unit, name: Meter, power: 2}]},
      {composite: false, units: [{prefix: Unit, name: Inch, power: 2}]},
      {composite: false, units: [{prefix: Unit, name: Foot, power: 2}]},
    ],
  },
  {
    dimensions: {
      length: 3,
      time: 0,
      mass: 0,
      ampere: 0,
      substance: 0,
      memory: 0,
      temperature: 0,
    },
    units: [
      {composite: false, units: [{prefix: Centi, name: Meter, power: 3}]},
      {composite: false, units: [{prefix: Unit, name: Meter, power: 3}]},
      {composite: false, units: [{prefix: Unit, name: Inch, power: 3}]},
      {composite: false, units: [{prefix: Unit, name: Foot, power: 3}]},
      {composite: false, units: [{prefix: Milli, name: Liter, power: 1}]},
      {composite: false, units: [{prefix: Centi, name: Liter, power: 1}]},
      {composite: false, units: [{prefix: Unit, name: Liter, power: 1}]},
      {composite: false, units: [{prefix: Unit, name: Gallon, power: 1}]},
      {composite: false, units: [{prefix: Unit, name: USGallon, power: 1}]},
    ],
  },
  {
    dimensions: {
      length: 0,
      time: 1,
      mass: 0,
      ampere: 0,
      substance: 0,
      memory: 0,
      temperature: 0,
    },
    units: [
      {composite: false, units: [{prefix: Milli, name: Second, power: 1}]},
      {composite: false, units: [{prefix: Unit, name: Second, power: 1}]},
      {composite: false, units: [{prefix: Unit, name: Minute, power: 1}]},
      {composite: false, units: [{prefix: Unit, name: Hour, power: 1}]},
      {composite: false, units: [{prefix: Unit, name: Day, power: 1}]},
      {composite: false, units: [{prefix: Unit, name: Week, power: 1}]},
      {composite: false, units: [{prefix: Unit, name: Month, power: 1}]},
      {composite: false, units: [{prefix: Unit, name: Year, power: 1}]},
    ],
  },
  {
    dimensions: {
      length: 0,
      time: 0,
      mass: 1,
      ampere: 0,
      substance: 0,
      memory: 0,
      temperature: 0,
    },
    units: [
      {composite: false, units: [{prefix: Unit, name: Gram, power: 1}]},
      {composite: false, units: [{prefix: Kilo, name: Gram, power: 1}]},
      {composite: false, units: [{prefix: Unit, name: Pound, power: 1}]},
      {composite: false, units: [{prefix: Unit, name: Ounce, power: 1}]},
      {
        composite: true,
        units: [{prefix: Unit, name: Pound, power: 1}, {prefix: Unit, name: Ounce, power: 1}],
      },
    ],
  },
  {
    dimensions: {
      length: 1,
      time: -1,
      mass: 0,
      ampere: 0,
      substance: 0,
      memory: 0,
      temperature: 0,
    },
    units: [
      {
        composite: false,
        units: [{prefix: Unit, name: Mile, power: 1}, {prefix: Unit, name: Hour, power: -1}],
      },
      {
        composite: false,
        units: [{prefix: Kilo, name: Meter, power: 1}, {prefix: Unit, name: Hour, power: -1}],
      },
      {
        composite: false,
        units: [{prefix: Unit, name: Meter, power: 1}, {prefix: Unit, name: Second, power: -1}],
      },
    ],
  },
  {
    dimensions: {
      length: 0,
      time: 0,
      mass: 0,
      ampere: 0,
      substance: 0,
      memory: 0,
      temperature: 1,
    },
    units: [
      {composite: false, units: [{prefix: Unit, name: Celsius, power: 1}]},
      {composite: false, units: [{prefix: Unit, name: Fahrenheit, power: 1}]},
    ],
  },
]
