open TechniCalcCalculator.Encoding
open TechniCalcCalculator.Unit_Types

%%private(
  let unitToUint = (element: unitType): int =>
    Belt.Array.getExn(Encoding_UnitMap_Eval.mapping, Obj.magic(element))
)
%%private(
  let unitOfUint = (index: int): option<unitType> =>
    Belt.Array.get(Encoding_UnitMap_Eval.reverseMapping, index)
)

%%private(
  let prefixToUint = (element: prefix): int =>
    Belt.Array.getExn(Encoding_PrefixMap_Eval.mapping, Obj.magic(element))
)
%%private(
  let prefixOfUint = (index: int): option<prefix> =>
    Belt.Array.get(Encoding_PrefixMap_Eval.reverseMapping, index)
)

%%private(
  let encodeUnitPart = (. {prefix, unit, power}: unitPart) =>
    prefixToUint(prefix)->encodeUint ++ unitToUint(unit)->encodeUint ++ encodeInt(power)
)

%%private(
  let readUnitPower = (. reader): option<TechniCalcCalculator.Unit_Types.unitPart> =>
    switch (readUint(reader), readUint(reader), readInt(reader)) {
    | (Some(prefix), Some(unit), Some(power)) =>
      switch (prefixOfUint(prefix), unitOfUint(unit)) {
      | (Some(prefix), Some(unit)) => Some({prefix: prefix, unit: unit, power: power})
      | _ => None
      }
    | _ => None
    }
)

let encodeUnitParts = units => encodeArray(units, encodeUnitPart)

let readUnitParts = reader => readArray(reader, readUnitPower)
