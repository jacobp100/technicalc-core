open UrlSafeEncoding

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
  let encodeCaptureGroup = (~placeholder) =>
    switch placeholder {
    | None => encodeUint(0)
    | Some(placeholder) => encodeUint(1) ++ Encoding_Symbol.encode(placeholder)
    }
)

%%private(
  let readCaptureGroup = reader => {
    switch readUint(reader) {
    | Some(0) => Some(AST.CaptureGroupStart({placeholder: None}))
    | Some(1) =>
      switch Encoding_Symbol.read(reader) {
      | Some(placeholder) => Some(CaptureGroupStart({placeholder: Some(placeholder)}))
      | None => None
      }
    | _ => None
    }
  }
)

%%private(let encodeCustomAtom = (~symbol, ~value) => Encoding_Symbol.encode(symbol) ++ value)

%%private(
  let readCustomAtom = reader =>
    switch (Encoding_Symbol.read(reader), TechniCalcCalculator.Encoding_Value.read(reader)) {
    | (Some(symbol), Some(value)) =>
      let value = TechniCalcCalculator.Encoding_Value.encode(value)
      AST.ConstantS({symbol, value})->Some
    | _ => None
    }
)

%%private(
  let encodeElement = (element: AST.t) =>
    switch element {
    | UnitConversion(_) => ""
    | CaptureGroupStart({placeholder}) => encodeUint(260) ++ encodeCaptureGroup(~placeholder)
    | VariableS({id, name}) => encodeUint(261) ++ encodeString(id) ++ encodeString(name)
    | TableNS({numRows, numColumns}) =>
      encodeUint(262) ++ encodeUint(numRows) ++ encodeUint(numColumns)
    | ConstantS({symbol, value}) => encodeUint(263) ++ encodeCustomAtom(~symbol, ~value)
    | element => elementToUint(element)->encodeUint
    }
)

%%private(
  let readElement = reader =>
    switch readUint(reader) {
    /* Legacy encodings */
    | Some(256 | 257 | 258 | 259) => None
    | Some(260) => readCaptureGroup(reader)
    | Some(261) =>
      switch (readString(reader), readString(reader)) {
      | (Some(id), Some(name)) => Some(VariableS({id, name}))
      | _ => None
      }
    | Some(262) =>
      switch (readUint(reader), readUint(reader)) {
      | (Some(numRows), Some(numColumns)) => Some(TableNS({numRows, numColumns}))
      | _ => None
      }
    | Some(263) => readCustomAtom(reader)
    /* Legacy table encodings */
    | Some(54) => Some(TableNS({numRows: 2, numColumns: 2}))
    | Some(55) => Some(TableNS({numRows: 3, numColumns: 3}))
    | Some(78) => Some(TableNS({numRows: 2, numColumns: 1}))
    | Some(79) => Some(TableNS({numRows: 3, numColumns: 1}))
    /* Legacy Mfrac element - no easy conversion */
    | Some(85) => None
    /* Legacy Rem element */
    | Some(90) => Some(Percent)
    | Some(value) => elementOfUint(value)
    | None => None
    }
)

let encodeElements = (input: array<AST.t>) => encodeArray(input, encodeElement)

let readElements = reader => readArray(reader, readElement)
