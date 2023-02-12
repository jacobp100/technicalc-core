open Measure_Types
open Measure_Base

let mulReal = ({value, units}: t, b: Real.t) => ofReal(Real.mul(value, b), ~units)
let divReal = ({value, units}: t, b: Real.t) => ofReal(Real.div(value, b), ~units)
