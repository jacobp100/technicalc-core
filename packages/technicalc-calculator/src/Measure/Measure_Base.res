open Measure_Types

let nan = {value: Real.nan, units: []}

let ofReal = (value: Real.t, ~units: array<Units.t>) =>
  !Real.isNaN(value) ? {value, units: Units.flatten(units)} : {value, units: []}
