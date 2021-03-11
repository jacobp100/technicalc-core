open Unit_Types
open Unit_Dimensions

let unitsCompatible = (~fromUnits, ~toUnits) => {
  eq(ofUnitParts(fromUnits), ofUnitParts(toUnits))
}

let compositeUnitsCompatible = (~fromUnits, ~toUnits) =>
  switch Belt.Array.get(fromUnits, 0) {
  | Some(unitPart) =>
    let baseDimensions = ofUnitPart(unitPart)
    let unitPartValid = (. unitPart) =>
      unitPart.power == 1 && eq(ofUnitPart(unitPart), baseDimensions)

    baseDimensions.temperature == 0 &&
      (size(baseDimensions) == 1 &&
      (Belt.Array.everyU(fromUnits, unitPartValid) && Belt.Array.everyU(toUnits, unitPartValid)))
  | None => false
  }
