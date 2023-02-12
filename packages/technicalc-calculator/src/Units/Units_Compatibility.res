open Units_Types
open Units_Base

let compatible = (~fromUnits, ~toUnits) => {
  open Units_Dimensions
  eq(ofUnits(fromUnits), ofUnits(toUnits))
}

%%private(
  let unitsUnique = units => {
    let rec iter = (currentUnitIndex, previousUnitIndex) =>
      if currentUnitIndex >= Belt.Array.length(units) {
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

let compositeCompatible = (~fromUnits, ~toUnits) =>
  switch Belt.Array.get(fromUnits, 0) {
  | Some(unit) if Belt.Array.length(toUnits) != 0 =>
    let baseDimensions = Units_Dimensions.ofUnit(unit)
    let unitValid = (. unit) =>
      unit.power == 1 && Units_Dimensions.eq(Units_Dimensions.ofUnit(unit), baseDimensions)

    baseDimensions.temperature == 0 &&
    Units_Dimensions.size(baseDimensions) == 1 &&
    Belt.Array.everyU(fromUnits, unitValid) &&
    Belt.Array.everyU(toUnits, unitValid) &&
    unitsUnique(toUnits)
  | _ => false
  }
