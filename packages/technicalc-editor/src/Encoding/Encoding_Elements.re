open TechniCalcCalculator.Encoding;

let%private encodeUnitConversion = (~fromUnits, ~toUnits) =>
  Encoding_Units.encodeUnitPowers(fromUnits)
  ++ Encoding_Units.encodeUnitPowers(toUnits);

let%private readUnitConversion = reader =>
  switch (
    Encoding_Units.readUnitPowers(reader),
    Encoding_Units.readUnitPowers(reader),
  ) {
  | (Some(fromUnits), Some(toUnits)) =>
    Some(AST_Types.UnitConversion({fromUnits, toUnits}))
  | _ => None
  };

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
    AST_Types.CustomAtomS({mml, value})->Some;
  | _ => None
  };

let%private encodeElement =
  (. element: AST.t) =>
    switch (element) {
    | UnitConversion({fromUnits, toUnits}) =>
      encodeUint(256) ++ encodeUnitConversion(~fromUnits, ~toUnits)
    | CustomAtomS({mml, value}) =>
      encodeUint(257) ++ encodeCustomAtom(~mml, ~value)
    | LabelS({mml}) => encodeUint(258) ++ encodeString(mml)
    | VariableS(string) => encodeUint(259) ++ encodeString(string)
    | element => Encoding_Element.toUint(element)->encodeUint
    };

let%private readElement =
  (. reader) =>
    switch (readUint(reader)) {
    | Some(256) => readUnitConversion(reader)
    | Some(257) => readCustomAtom(reader)
    | Some(258) =>
      switch (readString(reader)) {
      | Some(mml) => Some(LabelS({mml: mml}))
      | None => None
      }
    | Some(259) =>
      switch (readString(reader)) {
      | Some(string) => Some(VariableS(string))
      | None => None
      }
    | Some(value) => Encoding_Element.ofUint(value)
    | None => None
    };

let encodeElements = (input: array(AST.t)) =>
  encodeArray(input, encodeElement);

let readElements = reader => readArray(reader, readElement);
