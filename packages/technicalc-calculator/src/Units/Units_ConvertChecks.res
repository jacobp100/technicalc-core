open Unit_Types
open Unit_Dimensions

let unitsCompatible = (~fromUnits, ~toUnits) => {
  eq(ofUnits(fromUnits), ofUnits(toUnits))
}

let compositeUnitsCompatible = (~fromUnits, ~toUnits) =>
  switch Belt.Array.get(fromUnits, 0) {
  | Some(unit) =>
    let baseDimensions = ofUnit(unit)
    let unitValid = (. unit) => unit.power == 1 && eq(ofUnit(unit), baseDimensions)

    baseDimensions.temperature == 0 &&
      (size(baseDimensions) == 1 &&
      (Belt.Array.everyU(fromUnits, unitValid) && Belt.Array.everyU(toUnits, unitValid)))
  | None => false
  }
