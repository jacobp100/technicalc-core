open Unit_Types

let unitsCompatible = (~fromUnits, ~toUnits) => {
  open Unit_Dimensions
  eq(ofUnits(fromUnits), ofUnits(toUnits))
}

%%private(
  let unitsUnique = units => {
    let rec iter = (currentUnitIndex, previousUnitIndex) =>
      if currentUnitIndex === Belt.Array.length(units) {
        true
      } else if previousUnitIndex >= currentUnitIndex {
        iter(currentUnitIndex + 1, 0)
      } else if (
        !eq(
          Belt.Array.getUnsafe(units, currentUnitIndex),
          Belt.Array.getUnsafe(units, previousUnitIndex),
        )
      ) {
        iter(currentUnitIndex, previousUnitIndex + 1)
      } else {
        false
      }
    iter(1, 0)
  }
)

let compositeUnitsCompatible = (~fromUnits, ~toUnits) =>
  switch Belt.Array.get(fromUnits, 0) {
  | Some(unit) =>
    let baseDimensions = Unit_Dimensions.ofUnit(unit)
    let unitValid = (. unit) =>
      unit.power == 1 && Unit_Dimensions.eq(Unit_Dimensions.ofUnit(unit), baseDimensions)

    baseDimensions.temperature == 0 &&
    Unit_Dimensions.size(baseDimensions) == 1 &&
    Belt.Array.everyU(fromUnits, unitValid) &&
    Belt.Array.everyU(toUnits, unitValid) &&
    unitsUnique(toUnits)
  | None => false
  }
