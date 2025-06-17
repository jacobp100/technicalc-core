open Measure_Types
open Measure_Base

type imperialUnit = | @as(2) Inch | @as(1) Foot | @as(0) Yard

external imperialUnitPrecision: imperialUnit => int = "%identity"

%%private(
  let mostPreciseImperialUnit = (a, b) =>
    imperialUnitPrecision(a) > imperialUnitPrecision(b) ? a : b
)

%%private(
  let imperialUnit = (units: array<Units.t>) => {
    let rec iter = (~imperialUnit, i) =>
      switch Belt.Array.get(units, i) {
      | Some({name: (Inch | Foot | Yard) as name}) =>
        let nextImperialUnit = switch name {
        | Foot => Foot
        | Yard => Yard
        | _ => Inch
        }
        switch imperialUnit {
        | Some(imperialUnit) =>
          let imperialUnit = mostPreciseImperialUnit(imperialUnit, nextImperialUnit)->Some
          iter(~imperialUnit, i + 1)
        | None => iter(~imperialUnit=Some(nextImperialUnit), i + 1)
        }
      | Some(_) => None
      | None => imperialUnit
      }
    iter(~imperialUnit=None, 0)
  }
)

%%private(
  let toCompatibleUnits2 = (a, b) => {
    if Belt.Array.eq(a.units, b.units, (a, b) => Units.eq(a, b)) {
      Some((a.value, b.value, a.units))
    } else if Units.compatible(~fromUnits=a.units, ~toUnits=b.units) {
      let imperialUnit = switch (imperialUnit(a.units), imperialUnit(b.units)) {
      | (Some(a), Some(b)) => mostPreciseImperialUnit(a, b)->Some
      | (_, _) => None
      }
      let lengthUnit: option<Units.name> = switch imperialUnit {
      | Some(Inch) => Some(Inch)
      | Some(Foot) => Some(Foot)
      | Some(Yard) => Some(Yard)
      | None => None
      }
      let siUnits = Belt.Array.flatMap(a.units, u => Units.toSi(~lengthUnit?, u))
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

%%private(
  let mulUnitPowers = (units: array<Units.t>, power: int) =>
    Belt.Array.map(units, (u: Units.t) => {...u, power: u.power * power})
)

%%private(
  let hasOverlappingUnits = (units: array<Units.t>) =>
    if Belt.Array.length(units) < 2 {
      false
    } else {
      let dimensionSizes = Belt.Array.map(units, unit => {
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
      let siUnits = Belt.Array.flatMap(units, unit => Units.toSi(unit))->Units.flatten
      let siValue = Units.convert(value, ~fromUnits=units, ~toUnits=siUnits)
      ofReal(siValue, ~units=siUnits)
    } else {
      ofReal(value, ~units)
    }
)

let mul = (a: t, b: t): t =>
  normalized(Real.mul(a.value, b.value), ~units=Belt.Array.concat(a.units, b.units)->Units.flatten)

let div = (a: t, b: t): t =>
  normalized(
    Real.div(a.value, b.value),
    ~units=Belt.Array.concat(a.units, mulUnitPowers(b.units, -1))->Units.flatten,
  )

let powInt = (a: t, b: int) => normalized(Real.powInt(a.value, b), ~units=mulUnitPowers(a.units, b))
