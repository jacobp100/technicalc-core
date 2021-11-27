@scope("Object") @val external jsDictEntries: Js.Dict.t<'t> => array<(string, 't)> = "entries"

%%private(
  let toJsResult = result =>
    switch result {
    | Ok(value) => Obj.magic({"ok": true, "value": value})
    | Error(error) => Obj.magic({"ok": false, "error": error})
    }
)

type config = {angleMode: option<string>}

type format = {
  mode: option<string>,
  style: option<string>,
  locale: option<string>,
  base: option<int>,
  precision: option<int>,
  digitGrouping: option<bool>,
  decimalMinMagnitude: option<int>,
  decimalMaxMagnitude: option<int>,
}

// Should be private
// https://github.com/rescript-lang/rescript-compiler/issues/4958
let emptyFormat = {
  mode: None,
  style: None,
  locale: None,
  base: None,
  precision: None,
  digitGrouping: None,
  decimalMinMagnitude: None,
  decimalMaxMagnitude: None,
}

%%private(let defaultFormat = TechniCalcCalculator.Formatting_Types.defaultFormat)

%%private(
  @inline
  let formatStyle = f =>
    switch f.style {
    | Some("decimal") => TechniCalcCalculator.Formatting.Decimal
    | Some("engineering") => Engineering
    | Some("natural-mixed") => Natural({mixedFractions: true})
    | _ => defaultFormat.style
    }
)
%%private(
  @inline
  let formatLocale = f =>
    switch f.locale {
    | Some("european") => TechniCalcCalculator.Formatting.European
    | _ => defaultFormat.locale
    }
)
%%private(
  @inline
  let formatBase = f =>
    switch f.base {
    | Some(base) => base
    | None => defaultFormat.base
    }
)
%%private(
  @inline
  let formatPrecision = f =>
    switch f.precision {
    | Some(precision) => precision
    | None => defaultFormat.precision
    }
)
%%private(
  @inline
  let formatDigitGrouping = f =>
    switch f.digitGrouping {
    | Some(digitGrouping) => digitGrouping
    | None => defaultFormat.digitGrouping
    }
)
%%private(
  @inline
  let formatDecimalMinMagnitude = f =>
    switch f.decimalMinMagnitude {
    | Some(decimalMinMagnitude) => decimalMinMagnitude
    | None => defaultFormat.decimalMinMagnitude
    }
)
%%private(
  @inline
  let formatDecimalMaxMagnitude = f =>
    switch f.decimalMaxMagnitude {
    | Some(decimalMaxMagnitude) => decimalMaxMagnitude
    | None => defaultFormat.decimalMaxMagnitude
    }
)

module Elements = {
  open TechniCalcEditor

  let ofString = AST.ofString

  let encode = Encoding.encode
  let decode = Encoding.decode

  let toMml = (x, maybeFormat, maybeInline) => {
    let locale = switch maybeFormat {
    | Some(format) => formatLocale(format)
    | None => defaultFormat.locale
    }
    let locale = switch locale {
    | English => Mml.English
    | European => European
    }
    let digitGrouping = switch maybeFormat {
    | Some(format) => formatDigitGrouping(format)
    | None => defaultFormat.digitGrouping
    }
    let inline = switch maybeInline {
    | Some(inline) => inline
    | None => false
    }
    Mml.create(~locale, ~digitGrouping, ~inline, x)
  }

  let parse = elements => Value.parse(elements)->toJsResult

  let eq = (a, b) => Belt.Array.eqU(a, b, (. a, b) => AST.eq(a, b))

  let bracketRanges = BracketMetadata.bracketRanges
  let bracketRange = BracketMetadata.bracketRange

  let captureGroups = CaptureGroupMetadata.captureGroups

  let getDependencies = DependencyMetadata.getDependencies

  let inputSettingsMode = InputConfigMetadata.inputSettingsMode

  let insertRanges = InsertMetadata.insertRanges
  let canInsertTable = InsertMetadata.canInsertTable
  let canInsertIteration = InsertMetadata.canInsertIteration

  let renameVariables = RenameVariables.renameVariables
}

module Editor = {
  open TechniCalcEditor

  let empty = EditState.empty
  let make = EditState.make

  let clear = EditState.clear

  let setIndex = EditState.setIndex
  let previous = EditState.previous
  let next = EditState.next
  let moveStart = EditState.moveStart
  let moveEnd = EditState.moveEnd

  let insert = (editState, key) =>
    switch key {
    | Keys.One(element) => EditState.insert(editState, element)
    | Many(elements) => EditState.insertArray(editState, elements)
    }
  let delete = EditState.delete
}

module RPN = {
  open TechniCalcEditor

  let empty = RPN.empty

  let elements = RPN.elements
  let depth = RPN.depth
  let stack = RPN.stack

  let swap = RPN.swap
  let drop = RPN.drop

  let submit = (rpn, editState) => RPN.submit(rpn, editState)->toJsResult
  let insert = (rpn, editState, key) =>
    switch key {
    | Keys.One(element) => RPN.insert(rpn, editState, element)
    | Many(elements) => RPN.insertArray(rpn, editState, elements)
    }->toJsResult
}

module Keys = {
  open TechniCalcEditor

  let keys = Keys.keys

  let variable = (~id, ~name) => Keys.One(VariableS({id: id, name: name}))

  let customAtom = (~value, ~mml) =>
    AST.CustomAtomS({
      mml: mml,
      value: TechniCalcCalculator.Encoding.encode(value),
    })->Keys.One

  let label = (~placeholderMml) => Keys.Many([
    CaptureGroupStart({placeholderMml: placeholderMml}),
    CaptureGroupEndS,
  ])

  let equation = (~elements) => Keys.Many(elements)

  let elements = key =>
    switch key {
    | Keys.One(element) => [element]
    | Keys.Many(elements) => elements
    }
}

module Value = {
  open TechniCalcCalculator

  %%private(
    let getFormat = (~mode, maybeFormat: option<format>) =>
      switch maybeFormat {
      | None => {...Formatting.defaultFormat, mode: mode}
      | Some(format) => {
          mode: mode,
          style: formatStyle(format),
          locale: formatLocale(format),
          base: formatBase(format),
          precision: formatPrecision(format),
          digitGrouping: formatDigitGrouping(format),
          decimalMinMagnitude: formatDecimalMinMagnitude(format),
          decimalMaxMagnitude: formatDecimalMaxMagnitude(format),
        }
      }
  )

  let encode = Encoding.encode
  let decode = encoded =>
    switch Encoding.decode(encoded) {
    | Some(value) => value
    | None => Value_Base.nan
    }

  let nan = Value_Base.nan
  let isNaN = Value_Base.isNaN

  let toString = x => Formatting.toString(x)
  let ofString = x =>
    switch Formatting.ofString(x) {
    | Some(value) => value
    | None => Value_Base.nan
    }

  let toUnicode = (x, maybeFormat) => {
    let format = getFormat(~mode=Unicode, maybeFormat)
    Formatting.toString(~format, x)
  }

  let toMml = (x, maybeFormat, maybeInline) => {
    let format = getFormat(~mode=MathML, maybeFormat)
    let inline = Belt.Option.getWithDefault(maybeInline, false)
    Formatting.toString(~format, ~inline, x)
  }
}

module Work = {
  let calculate = (body): Work.input<'a> => Calculate(body)
  let convertUnits = (body, fromUnits, toUnits): Work.input<'a> => ConvertUnits({
    body: body,
    fromUnits: fromUnits,
    toUnits: toUnits,
  })
  let convertUnitsComposite = (values, toUnits): Work.input<'a> => ConvertUnitsComposite({
    values: values,
    toUnits: toUnits,
  })
  let solveRoot = (lhs, rhs, initialGuess): Work.input<'a> => {
    let body = TechniCalcCalculator.AST_Types.Sub(lhs, rhs)
    SolveRoot({body: body, initialGuess: initialGuess})
  }
  let quadratic = (a, b, c): Work.input<'a> => Quadratic(a, b, c)
  let cubic = (a, b, c, d): Work.input<'a> => Cubic(a, b, c, d)
  let var2 = (x0, y0, c0, x1, y1, c1): Work.input<'a> => Var2(x0, y0, c0, x1, y1, c1)
  let var3 = (x0, y0, z0, c0, x1, y1, z1, c1, x2, y2, z2, c2): Work.input<'a> => Var3(
    x0,
    y0,
    z0,
    c0,
    x1,
    y1,
    z1,
    c1,
    x2,
    y2,
    z2,
    c2,
  )

  let make = (config, context, input): Work.t<'a> => {
    config: {
      angleMode: switch config.angleMode {
      | Some("degree") => Degree
      | Some("gradian") => Gradian
      | _ => Radian
      },
    },
    context: switch context {
    | Some(context) =>
      jsDictEntries(context)->Belt.Array.mapU((. (key, value)) => (key, Value.encode(value)))
    | None => []
    },
    input: input,
  }

  let decodeOutput = Work.decodeOutput
}

module Units = {
  // let encodeUnitParts = TechniCalcEditor.Encoding_Units.encodeUnitParts;
  // let decodeUnitParts = encoded =>
  //   TechniCalcCalculator.Encoding.read(
  //     encoded,
  //     TechniCalcEditor.Encoding_Units.readUnitParts,
  //   );

  let unitsCompatible = TechniCalcCalculator.Units_ConvertChecks.unitsCompatible
  let compositeUnitsCompatible = TechniCalcCalculator.Units_ConvertChecks.compositeUnitsCompatible

  let toMml = TechniCalcEditor.Mml_Units.toMml
  let toString = Units_Util.toString

  let prefixes = Units_Util.prefixes

  let prefixToString = Units_Util.prefixToString
  let unitTypeToString = Units_Util.unitTypeToString

  let prefixToMml = Units_Util.prefixToMml
}
