open Unit_Types;

let unitsCompatible = (~fromUnits, ~toUnits) =>
  Unit_Dimensions.(equal(ofUnitParts(fromUnits), ofUnitParts(toUnits)));

let compositeUnitsCompatible = (~fromUnits, ~toUnits) =>
  switch (Belt.Array.get(fromUnits, 0)) {
  | Some(unitPart) =>
    let baseDimensions = Unit_Dimensions.ofUnitPart(unitPart);
    let unitPartValid = (
      (. unitPart) => {
        unitPart.power == 1
        && Unit_Dimensions.(equal(ofUnitPart(unitPart), baseDimensions));
      }
    );

    baseDimensions.temperature == 0
    && Unit_Dimensions.size(baseDimensions) == 1
    && Belt.Array.everyU(fromUnits, unitPartValid)
    && Belt.Array.everyU(toUnits, unitPartValid);
  | None => false
  };
