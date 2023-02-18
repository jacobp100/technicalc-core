open Measure_Types

let nan = {value: Real.nan, units: Units.empty}

let ofReal = (value: Real.t, ~units: Units.t) => !Real.isNaN(value) ? {value, units} : nan
