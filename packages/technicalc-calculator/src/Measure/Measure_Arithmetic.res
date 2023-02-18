open Measure_Types
open Measure_Base

%%private(
  let toCompatibleUnits2 = (a, b) => {
    let aUnits = Units.toArray(a.units)
    let bUnits = Units.toArray(b.units)

    if Belt.Array.eqU(aUnits, bUnits, (. a, b) => Unit.eq(a, b)) {
      Some((a.value, b.value, a.units))
    } else if Units.compatible(~fromUnits=a.units, ~toUnits=b.units) {
      let siUnits = Belt.Array.flatMapU(aUnits, (. u) => Unit.toSi(u))->Units.ofArray
      let aValue = Units.convert(~fromUnits=a.units, ~toUnits=siUnits, a.value)
      let bValue = Units.convert(~fromUnits=b.units, ~toUnits=siUnits, b.value)
      Some((aValue, bValue, siUnits))
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
  ofReal(
    Real.mul(a.value, b.value),
    ~units=Belt.Array.concat(Units.toArray(a.units), Units.toArray(b.units))->Units.ofArray,
  )

%%private(
  let mulUnitPowers = (units: array<Unit.t>, power: int) =>
    Belt.Array.mapU(units, (. u) => {...u, power: u.power * power})
)

let div = (a: t, b: t): t =>
  ofReal(
    Real.mul(a.value, b.value),
    ~units=Belt.Array.concat(
      Units.toArray(a.units),
      mulUnitPowers(Units.toArray(b.units), -1),
    )->Units.ofArray,
  )

let powInt = (a: t, b: int) =>
  ofReal(Real.powInt(a.value, b), ~units=Units.toArray(a.units)->mulUnitPowers(b)->Units.ofArray)
