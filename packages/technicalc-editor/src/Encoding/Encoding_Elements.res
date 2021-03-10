open TechniCalcCalculator.Encoding

%%private(
  let elementToUint = (element: AST.t): int =>
    Belt.Array.getExn(Encoding_ElementMap_Eval.mapping, Obj.magic(element))
)
%%private(
  let elementOfUint = (index: int): option<AST.t> =>
    Belt.Array.get(Encoding_ElementMap_Eval.reverseMapping, index)
)

// let%private encodeUnitConversion = (~fromUnits, ~toUnits) =>
//   Encoding_Units.encodeUnitParts(fromUnits)
//   ++ Encoding_Units.encodeUnitParts(toUnits);

// let%private readUnitConversion = reader =>
//   switch (
//     Encoding_Units.readUnitParts(reader),
//     Encoding_Units.readUnitParts(reader),
//   ) {
//   | (Some(fromUnits), Some(toUnits)) =>
//     Some(AST.UnitConversion({fromUnits, toUnits}))
//   | _ => None
//   };

%%private(
  let encodeCustomAtom = (~mml, ~value) => encodeString(mml) ++ value
)

%%private(
  let readCustomAtom = reader =>
    switch (readString(reader), TechniCalcCalculator.Encoding_Value.readValue(reader)) {
    | (Some(mml), Some(value)) =>
      let value = TechniCalcCalculator.Encoding_Value.encodeValue(value)
      AST.CustomAtomS({mml: mml, value: value})->Some
    | _ => None
    }
)

%%private(
  let encodeElement = (. element: AST.t) =>
    switch element {
    | UnitConversion(_) => ""
    | CustomAtomS({mml, value}) => encodeUint(257) ++ encodeCustomAtom(~mml, ~value)
    | VariableS({id, name}) => encodeUint(261) ++ (encodeString(id) ++ encodeString(name))
    | CaptureGroupStart({placeholderMml}) => encodeUint(260) ++ encodeString(placeholderMml)
    | element => elementToUint(element)->encodeUint
    }
)

%%private(
  let readElement = (. reader) =>
    switch readUint(reader) {
    | Some(256) => None
    | Some(257) => readCustomAtom(reader)
    | Some(261) =>
      switch (readString(reader), readString(reader)) {
      | (Some(id), Some(name)) => Some(VariableS({id: id, name: name}))
      | _ => None
      }
    | Some(260) =>
      switch readString(reader) {
      | Some(placeholderMml) => Some(CaptureGroupStart({placeholderMml: placeholderMml}))
      | None => None
      }
    | Some(value) => elementOfUint(value)
    | None => None
    }
)

let encodeElements = (input: array<AST.t>) => encodeArray(input, encodeElement)

let readElements = reader => readArray(reader, readElement)
