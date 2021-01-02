open TechniCalcCalculator.Encoding;

let%private encodeUnitPart =
  (. {prefix, unit, power}: TechniCalcCalculator.Unit_Types.unitPart) =>
    Encoding_Prefix.toUint(prefix)->encodeUint
    ++ Encoding_Unit.toUint(unit)->encodeUint
    ++ encodeInt(power);

let%private readUnitPower =
  (. reader) => (
    switch (readUint(reader), readUint(reader), readInt(reader)) {
    | (Some(prefix), Some(unit), Some(power)) =>
      switch (Encoding_Prefix.ofUint(prefix), Encoding_Unit.ofUint(unit)) {
      | (Some(prefix), Some(unit)) => Some({prefix, unit, power})
      | _ => None
      }
    | _ => None
    }:
      option(TechniCalcCalculator.Unit_Types.unitPart)
  );

let encodeUnitParts = units => encodeArray(units, encodeUnitPart);

let readUnitParts = reader => readArray(reader, readUnitPower);
