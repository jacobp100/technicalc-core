open Measure_Types
open Measure_Base

%%private(
  let toCompatibleUnits2 = (a, b) => {
    if Belt.Array.eqU(a.units, b.units, (. a, b) => Units.eq(a, b)) {
      Some((a.value, b.value, a.units))
    } else if Units.compatible(~fromUnits=a.units, ~toUnits=b.units) {
      let aUnits = Belt.Array.flatMapU(a.units, (. u) => Units.toSi(u))
      let bUnits = Belt.Array.flatMapU(b.units, (. u) => Units.toSi(u))
      assert Units.compatible(~fromUnits=aUnits, ~toUnits=bUnits)
      let aValue = Units.convert(~fromUnits=a.units, ~toUnits=aUnits, a.value)
      let bValue = Units.convert(~fromUnits=b.units, ~toUnits=bUnits, b.value)
      Some((aValue, bValue, aUnits))
    } else {
      None
    }
  }
)

let neg = (a: t) => ofReal(Real.neg(a.value), ~units=a.units)

let add = (a: t, b: t): t =>
  switch toCompatibleUnits2(a, b) {
  | Some((a, b, units)) => ofReal(Real.add(a, b), ~units)
  | None => nan
  }

let sub = (a: t, b: t): t =>
  switch toCompatibleUnits2(a, b) {
  | Some((a, b, units)) => ofReal(Real.sub(a, b), ~units)
  | None => nan
  }

let mul = (a: t, b: t): t =>
  ofReal(Real.mul(a.value, b.value), ~units=Belt.Array.concat(a.units, b.units)->Units.flatten)

%%private(
  let mulUnitPowers = (units: array<Units.t>, power: int) =>
    Belt.Array.mapU(units, (. u: Units.t) => {...u, power: u.power * power})
)

let div = (a: t, b: t): t =>
  ofReal(
    Real.mul(a.value, b.value),
    ~units=Belt.Array.concat(a.units, mulUnitPowers(b.units, -1))->Units.flatten,
  )

let powInt = (a: t, b: int) => ofReal(Real.powInt(a.value, b), ~units=mulUnitPowers(a.units, b))
