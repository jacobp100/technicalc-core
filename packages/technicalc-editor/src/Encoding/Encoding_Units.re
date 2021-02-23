open TechniCalcCalculator.Encoding;
open TechniCalcCalculator.Unit_Types;

let%private unitToUint = (element: unitType): int =>
  Belt.Array.getExn(Encoding_UnitMap_Eval.mapping, Obj.magic(element));
let%private unitOfUint = (index: int): option(unitType) =>
  Belt.Array.get(Encoding_UnitMap_Eval.reverseMapping, index);

let%private prefixToUint = (element: prefix): int =>
  Belt.Array.getExn(Encoding_PrefixMap_Eval.mapping, Obj.magic(element));
let%private prefixOfUint = (index: int): option(prefix) =>
  Belt.Array.get(Encoding_PrefixMap_Eval.reverseMapping, index);

let%private encodeUnitPart =
  (. {prefix, unit, power}: unitPart) =>
    prefixToUint(prefix)->encodeUint
    ++ unitToUint(unit)->encodeUint
    ++ encodeInt(power);

let%private readUnitPower =
  (. reader) => (
    switch (readUint(reader), readUint(reader), readInt(reader)) {
    | (Some(prefix), Some(unit), Some(power)) =>
      switch (prefixOfUint(prefix), unitOfUint(unit)) {
      | (Some(prefix), Some(unit)) => Some({prefix, unit, power})
      | _ => None
      }
    | _ => None
    }:
      option(TechniCalcCalculator.Unit_Types.unitPart)
  );

let encodeUnitParts = units => encodeArray(units, encodeUnitPart);

let readUnitParts = reader => readArray(reader, readUnitPower);
