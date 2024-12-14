open Measure_Types
open Measure_Base

%%private(let negatePower = (x: Units.t) => {...x, power: -x.power})

let mulReal = ({value, units}: t, real: Real.t) => ofReal(Real.mul(value, real), ~units)
let divReal = ({value, units}: t, real: Real.t) => ofReal(Real.div(value, real), ~units)
let realDiv = (real: Real.t, {value, units}: t) =>
  ofReal(Real.div(real, value), ~units=Belt.Array.map(units, negatePower))
