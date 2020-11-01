open TechniCalcCalculator.Encoding;

let%private encodeUnitPower =
  (. (unitType, power): TechniCalcCalculator.Unit_Types.unitPower) =>
    Encoding_Unit.toInt(unitType)->encodeUint ++ encodeInt(power);

let%private readUnitPower =
  (. reader) =>
    switch (readUint(reader), readInt(reader)) {
    | (Some(unit), Some(power)) =>
      switch (Encoding_Unit.ofInt(unit)) {
      | Some(unit) =>
        Some((unit, power): TechniCalcCalculator.Unit_Types.unitPower)
      | None => None
      }
    | _ => None
    };

let encodeUnitPowers = units => encodeArray(units, encodeUnitPower);

let readUnitPowers = reader => readArray(reader, readUnitPower);
