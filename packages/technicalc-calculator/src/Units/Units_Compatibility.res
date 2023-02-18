open Units_Types

let compatible = (~fromUnits, ~toUnits) => {
  open Units_Dimensions
  Unit.dimensionsEq(dimensions(fromUnits), dimensions(toUnits))
}

%%private(
  let unitsUnique = x => {
    let rec iter = (currentUnitIndex, previousUnitIndex) =>
      if currentUnitIndex >= Belt.Array.length(x.units) {
        true
      } else if previousUnitIndex >= currentUnitIndex {
        iter(currentUnitIndex + 1, 0)
      } else if (
        !Unit.eq(
          Belt.Array.getUnsafe(x.units, currentUnitIndex),
          Belt.Array.getUnsafe(x.units, previousUnitIndex),
        )
      ) {
        iter(currentUnitIndex, previousUnitIndex + 1)
      } else {
        false
      }
    iter(1, 0)
  }
)

let compositeCompatible = (~fromUnits, ~toUnits) =>
  switch Belt.Array.get(fromUnits.units, 0) {
  | Some(unit) if Belt.Array.length(toUnits.units) != 0 =>
    let baseDimensions = Unit.dimensions(unit)
    let unitValid = (. unit: Unit.t) =>
      unit.power == 1 && Unit.dimensionsEq(Unit.dimensions(unit), baseDimensions)

    baseDimensions.temperature == 0 &&
    Unit.dimensionsSize(baseDimensions) == 1 &&
    Belt.Array.everyU(fromUnits.units, unitValid) &&
    Belt.Array.everyU(toUnits.units, unitValid) &&
    unitsUnique(toUnits)
  | _ => false
  }
