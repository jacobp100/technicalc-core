open UrlSafeEncoding

%%private(
  let elementToUint = (element: AST.t): int =>
    Belt.Array.getExn(Encoding_ElementMap_Eval.mapping, Obj.magic(element))
)
%%private(
  let elementOfUint = (index: int): option<AST.t> =>
    Belt.Array.get(Encoding_ElementMap_Eval.reverseMapping, index)
)

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

%%private(let encodeConstant = (~symbol, ~value) => Encoding_Symbol.encode(symbol) ++ value)

%%private(
  let readConstant = reader =>
    switch (Encoding_Symbol.read(reader), TechniCalcCalculator.Encoding_Value.read(reader)) {
    | (Some(symbol), Some(value)) =>
      let value = TechniCalcCalculator.Encoding_Value.encode(value)
      AST.ConstantS({symbol, value})->Some
    | _ => None
    }
)

%%private(let encodeVariable = (~id, ~symbol) => encodeString(id) ++ Encoding_Symbol.encode(symbol))

%%private(
  let readVariable = reader =>
    switch (readString(reader), Encoding_Symbol.read(reader)) {
    | (Some(id), Some(symbol)) => AST.VariableS({id, symbol})->Some
    | _ => None
    }
)

%%private(
  let encodeUnit = (~prefix, ~name) =>
    // It's just easier to do this - adds one character to output
    TechniCalcCalculator.Encoding_Units.encodeUnit({prefix, name, power: 0})
)

%%private(
  let readUnit = reader =>
    switch TechniCalcCalculator.Encoding_Units.readUnit(reader) {
    | Some({prefix, name}) => AST.UnitS({prefix, name})->Some
    | None => None
    }
)

%%private(
  let encodeElement = (element: AST.t) =>
    switch element {
    | CaptureGroupStart({placeholder}) => encodeUint(260) ++ encodeCaptureGroup(~placeholder)
    | TableNS({numRows, numColumns}) =>
      encodeUint(262) ++ encodeUint(numRows) ++ encodeUint(numColumns)
    | ConstantS({symbol, value}) => encodeUint(263) ++ encodeConstant(~symbol, ~value)
    | VariableS({id, symbol}) => encodeUint(264) ++ encodeVariable(~id, ~symbol)
    | UnitS({prefix, name}) => encodeUint(265) ++ encodeUnit(~prefix, ~name)
    | element => elementToUint(element)->encodeUint
    }
)

%%private(
  let readElement = reader =>
    switch readUint(reader) {
    /* Legacy encodings */
    | Some(256 | 257 | 258 | 259) => None
    | Some(260) => readCaptureGroup(reader)
    | Some(262) =>
      switch (readUint(reader), readUint(reader)) {
      | (Some(numRows), Some(numColumns)) => Some(TableNS({numRows, numColumns}))
      | _ => None
      }
    | Some(263) => readConstant(reader)
    | Some(264) => readVariable(reader)
    | Some(265) => readUnit(reader)
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
