open UrlSafeEncoding
open TechniCalcCalculator.Unit_Types

%%private(
  let unitTypeToUint = (element: unitType): int =>
    Belt.Array.getExn(Encoding_UnitMap_Eval.mapping, Obj.magic(element))
)
%%private(
  let unitTypeOfUint = (index: int): option<unitType> =>
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
  let encodeUnit = ({prefix, type_, power}: t) =>
    prefixToUint(prefix)->encodeUint ++ unitTypeToUint(type_)->encodeUint ++ encodeInt(power)
)

%%private(
  let readUnit = (reader): option<t> =>
    switch (readUint(reader), readUint(reader), readInt(reader)) {
    | (Some(prefix), Some(type_), Some(power)) =>
      switch (prefixOfUint(prefix), unitTypeOfUint(type_)) {
      | (Some(prefix), Some(type_)) => Some({prefix, type_, power})
      | _ => None
      }
    | _ => None
    }
)

let encodeUnits = units => encodeArray(units, encodeUnit)

let readUnits = reader => readArray(reader, readUnit)
