open TechniCalcCalculator.Units

let unitsReplacements = {
  "K": [{prefix: Unit, name: Kelvin, power: 1}],
  "s": [{prefix: Unit, name: Second, power: 1}],
  "Hz": [{prefix: Unit, name: Hertz, power: 1}],
  "MHz": [{prefix: Mega, name: Hertz, power: 1}],
  "kg": [{prefix: Kilo, name: Gram, power: 1}],
  "m": [{prefix: Unit, name: Meter, power: 1}],
  "fm": [{prefix: Femto, name: Meter, power: 1}],
  "c": [{prefix: Unit, name: Coulomb, power: 1}],
  "N": [{prefix: Unit, name: Newton, power: 1}],
  "Pa": [{prefix: Unit, name: Pascal, power: 1}],
  "J": [{prefix: Unit, name: Joule, power: 1}],
  "W": [{prefix: Unit, name: Watt, power: 1}],
  "MeV": [{prefix: Mega, name: ElectronVolt, power: 1}],
  "GeV": [{prefix: Giga, name: ElectronVolt, power: 1}],
  "eV": [{prefix: Unit, name: ElectronVolt, power: 1}],
  "ohm": [{prefix: Unit, name: Ohm, power: 1}],
  "A": [{prefix: Unit, name: Ampere, power: 1}],
  "V": [{prefix: Unit, name: Volt, power: 1}],
  "C": [{prefix: Unit, name: Coulomb, power: 1}],
  "F": [{prefix: Unit, name: Farad, power: 1}],
  "Wb": [{prefix: Unit, name: Weber, power: 1}],
  "T": [{prefix: Unit, name: Tesla, power: 1}],
  "S": [{prefix: Unit, name: Siemens, power: 1}],
  "mol": [{prefix: Unit, name: Mole, power: 1}],
  "H": [{prefix: Unit, name: Henry, power: 1}],
  // One unit uses this - just ignore it
  "sr": ([]: array<t>),
  // We don't include these units
  "E_h": ([]: array<t>),
  "u": ([]: array<t>),
  "lm": [{prefix: Unit, name: Henry, power: 1}],
  // Weird artefacts - unit not included anyway
  "C_90": ([]: array<t>),
  "MeV/c": ([]: array<t>),
  "(GeV/c": ([]: array<t>),
}
