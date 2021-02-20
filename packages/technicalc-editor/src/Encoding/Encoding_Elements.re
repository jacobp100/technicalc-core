open TechniCalcCalculator.Encoding;

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

let%private encodeCustomAtom = (~mml, ~value) =>
  encodeString(mml) ++ /* Already encoded */ value;

let%private readCustomAtom = reader =>
  switch (
    readString(reader),
    TechniCalcCalculator.Encoding_Value.readValue(reader),
  ) {
  | (Some(mml), Some(value)) =>
    // This is a round-trip, but it does check for validity
    // And is required to get the reader in the right position
    let value = TechniCalcCalculator.Encoding_Value.encodeValue(value);
    AST.CustomAtomS({mml, value})->Some;
  | _ => None
  };

let%private encodeElement =
  (. element: AST.t) =>
    switch (element) {
    // | UnitConversion({fromUnits, toUnits}) =>
    //   encodeUint(256) ++ encodeUnitConversion(~fromUnits, ~toUnits)
    | UnitConversion(_) => ""
    | CustomAtomS({mml, value}) =>
      encodeUint(257) ++ encodeCustomAtom(~mml, ~value)
    | VariableS({id, name}) =>
      encodeUint(261) ++ encodeString(id) ++ encodeString(name)
    | CaptureGroupStart({placeholderMml}) =>
      encodeUint(260) ++ encodeString(placeholderMml)
    | element => Encoding_Element.toUint(element)->encodeUint
    };

let%private readElement =
  (. reader) =>
    switch (readUint(reader)) {
    // | Some(256) => readUnitConversion(reader)
    | Some(256) => None
    | Some(257) => readCustomAtom(reader)
    | Some(261) =>
      switch (readString(reader), readString(reader)) {
      | (Some(id), Some(name)) => Some(VariableS({id, name}))
      | _ => None
      }
    | Some(260) =>
      switch (readString(reader)) {
      | Some(placeholderMml) =>
        Some(CaptureGroupStart({placeholderMml: placeholderMml}))
      | None => None
      }
    | Some(value) => Encoding_Element.ofUint(value)
    | None => None
    };

let encodeElements = (input: array(AST.t)) =>
  encodeArray(input, encodeElement);

let readElements = reader => readArray(reader, readElement);
