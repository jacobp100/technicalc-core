open Measure_Types
open Measure_Base

%%private(
  let toCompatibleUnits2 = (a, b) => {
    if Belt.Array.eqU(a.units, b.units, (. a, b) => Units.eq(a, b)) {
      Some((a.value, b.value, a.units))
    } else if Units.compatible(~fromUnits=a.units, ~toUnits=b.units) {
      let siUnits = Belt.Array.flatMapU(a.units, (. u) => Units.toSi(u))
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
  ofReal(Real.mul(a.value, b.value), ~units=Belt.Array.concat(a.units, b.units))

%%private(
  let mulUnitPowers = (units: array<Units.t>, power: int) =>
    Belt.Array.mapU(units, (. u: Units.t) => {...u, power: u.power * power})
)

%%private(
  let hasOverlappingUnits = (units: array<Units.t>) =>
    if Belt.Array.length(units) < 2 {
      false
    } else {
      let dimensionSizes = Belt.Array.mapU(units, (. unit) => {
        let dimensions = Units_Dimensions.ofUnit(unit)
        let size = Units_Dimensions.size(dimensions)
        (dimensions, size)
      })

      let rec iter = (
        ~combinedDimensions as previousDimensions,
        ~combinedSize as previousSize,
        i,
      ) =>
        switch Belt.Array.get(dimensionSizes, i) {
        | Some((currentDimensions, currentSize)) =>
          let combinedDimensions = Units_Dimensions.combine(previousDimensions, currentDimensions)
          let combinedSize = Units_Dimensions.size(combinedDimensions)

          if combinedSize == currentSize + previousSize {
            iter(~combinedDimensions, ~combinedSize, i + 1)
          } else {
            true
          }
        | None => false
        }

      let (combinedDimensions, combinedSize) = Belt.Array.getExn(dimensionSizes, 0)
      iter(~combinedDimensions, ~combinedSize, 1)
    }

  let normalized = (value, ~units) =>
    if hasOverlappingUnits(units) {
      let siUnits = Belt.Array.flatMapU(units, (. unit) => Units.toSi(unit))->Units.flatten
      let siValue = Units.convert(value, ~fromUnits=units, ~toUnits=siUnits)
      ofReal(siValue, ~units=siUnits)
    } else {
      ofReal(value, ~units)
    }
)

let div = (a: t, b: t): t =>
  normalized(
    Real.div(a.value, b.value),
    ~units=Belt.Array.concat(a.units, mulUnitPowers(b.units, -1)),
  )

let powInt = (a: t, b: int) => normalized(Real.powInt(a.value, b), ~units=mulUnitPowers(a.units, b))
