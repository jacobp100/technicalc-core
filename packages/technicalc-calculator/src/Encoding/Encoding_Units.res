open UrlSafeEncoding
open Units_Types

%%private(
  let unitTypeToUint = (element: name): int =>
    Belt.Array.getExn(Encoding_UnitMap_Eval.mapping, Obj.magic(element))
)
%%private(
  let unitTypeOfUint = (index: int): option<name> =>
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
  let encodeUnit = ({prefix, name, power}: t) =>
    prefixToUint(prefix)->encodeUint ++ unitTypeToUint(name)->encodeUint ++ encodeInt(power)
)

%%private(
  let readUnit = (reader): option<t> =>
    switch (readUint(reader), readUint(reader), readInt(reader)) {
    | (Some(prefix), Some(name), Some(power)) =>
      switch (prefixOfUint(prefix), unitTypeOfUint(name)) {
      | (Some(prefix), Some(name)) => Some({prefix, name, power})
      | _ => None
      }
    | _ => None
    }
)

let encodeUnits = units => encodeArray(units, encodeUnit)

let readUnits = reader => readArray(reader, readUnit)
