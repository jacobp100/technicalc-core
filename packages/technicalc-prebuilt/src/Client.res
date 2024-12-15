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
  fractions: option<string>,
  exponents: option<string>,
  decimalSeparator: option<string>,
  groupingSeparator: option<string>,
  digitGrouping: option<bool>,
  base: option<int>,
  minDecimalPlaces: option<int>,
  maxDecimalPlaces: option<int>,
  decimalMinMagnitude: option<int>,
  decimalMaxMagnitude: option<int>,
}

// Should be private
// https://github.com/rescript-lang/rescript-compiler/issues/4958
let emptyFormat = {
  mode: None,
  fractions: None,
  exponents: None,
  decimalSeparator: None,
  groupingSeparator: None,
  base: None,
  minDecimalPlaces: None,
  maxDecimalPlaces: None,
  digitGrouping: None,
  decimalMinMagnitude: None,
  decimalMaxMagnitude: None,
}

%%private(let defaultFormat = TechniCalcCalculator.Formatting_Types.defaultFormat)

%%private(
  @inline
  let formatConstants = f =>
    switch f.constants {
    | Some(constants) => constants
    | _ => defaultFormat.constants
    }
)
%%private(
  @inline
  let formatFractions = f =>
    switch f.fractions {
    | Some("never") => TechniCalcCalculator.Formatting.Never
    | Some("improper") => TechniCalcCalculator.Formatting.Improper
    | Some("mixed") => TechniCalcCalculator.Formatting.Mixed
    | _ => defaultFormat.fractions
    }
)
%%private(
  @inline
  let formatExponents = f =>
    switch f.exponents {
    | Some("scientific") => TechniCalcCalculator.Formatting.Scientific
    | Some("engineering") => TechniCalcCalculator.Formatting.Engineering
    | _ => defaultFormat.exponents
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
  let formatMinDecimalPlaces = f =>
    switch f.minDecimalPlaces {
    | Some(minDecimalPlaces) => minDecimalPlaces
    | None => defaultFormat.minDecimalPlaces
    }
)
%%private(
  @inline
  let formatMaxDecimalPlaces = f =>
    switch f.maxDecimalPlaces {
    | Some(maxDecimalPlaces) => maxDecimalPlaces
    | None => defaultFormat.maxDecimalPlaces
    }
)
%%private(
  @inline
  let formatDecimalSeparator = f =>
    switch f.decimalSeparator {
    | Some(decimalSeparator) => decimalSeparator
    | None => defaultFormat.decimalSeparator
    }
)
%%private(
  @inline
  let formatGroupingSeparator = f =>
    switch f.groupingSeparator {
    | Some(groupingSeparator) => groupingSeparator
    | None => defaultFormat.groupingSeparator
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
%%private(
  @inline
  let formatUnitFormat = unitFormat =>
    switch unitFormat {
    | Some("operator") => TechniCalcCalculator.Formatting.Operator
    | _ => TechniCalcCalculator.Formatting.Exponential
    }
)

module Elements = {
  open TechniCalcEditor

  let ofString = AST.ofString
  let ofValue = AST.ofValue

  let encode = Encoding.encode
  let decode = x =>
    switch Encoding.decode(x) {
    | Some(ast) => Some(AST.normalize(ast))
    | None => None
    }

  %%private(
    let getFormat = (maybeFormat, ~metadata): TechniCalcEditor.Stringifier.format =>
      switch maybeFormat {
      | Some(format) => {
          decimalSeparator: formatDecimalSeparator(format),
          groupingSeparator: formatGroupingSeparator(format),
          digitGrouping: formatDigitGrouping(format),
          metadata,
        }
      | None => TechniCalcEditor.Stringifier.defaultFormat
      }
  )

  let toMml = (x, maybeFormat, maybeInline, maybeMetadata) => {
    let inline = switch maybeInline {
    | Some(inline) => inline
    | None => false
    }
    let metadata = switch maybeMetadata {
    | Some(metadata) => metadata
    | None => false
    }
    let format = getFormat(maybeFormat, ~metadata)
    Mml.create(~format, ~inline, x)
  }

  let toTex = (x, maybeFormat) => {
    let format = getFormat(maybeFormat, ~metadata=false)
    Tex.create(~format, x)
  }

  let parse = elements => Value.parse(elements)->toJsResult

  let eq = (a, b) => Belt.Array.eq(a, b, (a, b) => AST.eq(a, b))

  let baseRanges = BaseMetadata.baseRanges
  let baseRange = BaseMetadata.baseRange

  let bracketRanges = BracketMetadata.bracketRanges
  let bracketRange = BracketMetadata.bracketRange

  let placeholders = PlaceholderMetadata.placeholders

  let getDependencies = DependencyMetadata.getDependencies

  let hint = (elements, index) =>
    switch HintMetadata.hint(elements, index) {
    | Some(Value({symbol, value})) =>
      Some(Obj.magic({"type": "Value", "symbol": symbol, "value": value}))
    | Some(Variable({id})) => Some(Obj.magic({"type": "Variable", "id": id}))
    | Some(CaptureGroup({placeholder, isEmpty})) =>
      Some(Obj.magic({"type": "CaptureGroup", "placeholder": placeholder, "isEmpty": isEmpty}))
    | None => None
    }

  let inputSettingsMode = InputConfigMetadata.inputSettingsMode

  let insertRanges = InsertMetadata.insertRanges
  let canInsertTable = InsertMetadata.canInsertTable
  let canInsertIteration = InsertMetadata.canInsertIteration

  let iterationRanges = IterationMetadata.iterationRanges
  let insideIterator = IterationMetadata.insideIterator

  let tableRanges = TableMetadata.tableRanges
  let insideTable = TableMetadata.insideTable

  let reifyPlaceholders = ReifyPlaceholders.reifyPlaceholders

  let renameVariables = RenameVariables.renameVariables
}

module Editor = {
  open TechniCalcEditor

  let normalizeIfNeeded = ({elements, index, formatCaptureGroups}: EditState.t) => {
    let nextElements = AST.normalize(elements)
    if nextElements !== elements {
      Some(EditState.make(~index, ~elements=nextElements, ~formatCaptureGroups))
    } else {
      None
    }
  }

  let empty = EditState.empty
  let make = EditState.make

  let clear = EditState.clear

  let setIndex = EditState.setIndex
  let previous = EditState.previous
  let next = EditState.next
  let moveStart = EditState.moveStart
  let moveEnd = EditState.moveEnd
  let moveUp = EditState.moveUp
  let moveDown = EditState.moveDown

  let insert = (editState, key, config) =>
    switch key {
    | Keys.One(element) => EditState.insert(~config?, editState, element)
    | Many(elements) => EditState.insertArray(editState, elements)
    }
  let delete = EditState.delete
}

module Keys = {
  open TechniCalcEditor

  type unitType = {
    prefix: TechniCalcCalculator.Units.prefix,
    name: TechniCalcCalculator.Units.name,
    power: int,
  }

  let keys = Keys.keys

  let variable = (~id, ~symbol) => Keys.One(VariableS({id, symbol}))

  let constant = (~value, ~symbol) =>
    AST.ConstantS({
      symbol,
      value: TechniCalcCalculator.Encoding.encode(value),
    })->Keys.One

  %%private(
    let unit = ({prefix, name, power}): array<Keys.key> => {
      let unit: Keys.key = UnitS({prefix, name})
      if power != 1 {
        let power = Belt.Int.toString(power)->AST.ofString
        Belt.Array.concatMany([[unit, Superscript1], power, [Arg]])
      } else {
        [unit]
      }
    }
  )
  let units = (units: array<unitType>) => Keys.Many(Belt.Array.flatMap(units, unit))

  let table = (~numRows, ~numColumns) => Keys.One(TableNS({numRows, numColumns}))

  let captureGroup = (~placeholder) => Keys.Many([
    CaptureGroupStart({placeholder: placeholder}),
    CaptureGroupEndS,
  ])

  let equation = (~elements, ~symbol, ~inline) => {
    let inlineEquation = switch (
      symbol,
      inline ? EquationMetadata.equationMetadata(elements) : None,
    ) {
    | (Some(symbol), Some((elements, arguments))) =>
      switch TechniCalcEditor.Value.parse(elements) {
      | Ok(body) => Some(Keys.One(EquationNS({symbol, elements, body, arguments})))
      | Error(_) => None
      }
    | _ => None
    }
    switch inlineEquation {
    | Some(inlineEquation) => inlineEquation
    | _ => Keys.Many(elements)
    }
  }

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
      | None => {...Formatting.defaultFormat, mode}
      | Some(format) => {
          mode,
          constants: formatConstants(format),
          fractions: formatFractions(format),
          exponents: formatExponents(format),
          decimalSeparator: formatDecimalSeparator(format),
          groupingSeparator: formatGroupingSeparator(format),
          digitGrouping: formatDigitGrouping(format),
          base: formatBase(format),
          minDecimalPlaces: formatMinDecimalPlaces(format),
          maxDecimalPlaces: formatMaxDecimalPlaces(format),
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

  let ofString = x =>
    switch Formatting.ofString(x) {
    | Some(value) => value
    | None => Value_Base.nan
    }

  let toString = (x, maybeFormat) => {
    let format = getFormat(~mode=Ascii, maybeFormat)
    Formatting.toString(~format, x)
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

  let toTex = (x, maybeFormat) => {
    let format = getFormat(~mode=Tex, maybeFormat)
    Formatting.toString(~format, x)
  }

  let ofMeasure = (x: Value.t, units: array<Units.t>) =>
    Value_Base.toReal(x)->Measure_Base.ofReal(~units)->Value_Base.ofMeasure
  let toMeasure = (x: Value.t) =>
    switch x {
    | #Mesr({value, units}) => Some({"value": Scalar_Base.ofReal(value), "units": units})
    | _ => None
    }
}

module Work = {
  let calculate = (body): Work.input<'a> => Calculate(body)
  let convertUnits = (body, fromUnits, toUnits): Work.input<'a> => ConvertUnits({
    body,
    fromUnits,
    toUnits,
  })
  let convertUnitsComposite = (values, toUnits): Work.input<'a> => ConvertUnitsComposite({
    values,
    toUnits,
  })
  let convertCurrency = (body, fromCurrency, toCurrencies): Work.input<'a> => ConvertCurrencies({
    body,
    fromCurrency,
    toCurrencies,
  })
  let solveRoot = (lhs, rhs, initialGuess): Work.input<'a> => {
    let body = TechniCalcCalculator.AST_Types.Sub(lhs, rhs)
    SolveRoot({body, initialGuess})
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
    | Some(context) => jsDictEntries(context)
    | None => []
    },
    input,
  }

  let encodeInput = Work.encodeInput
  let decodeOutput = Work.decodeOutput
}

module Units = {
  open TechniCalcCalculator

  let eq = Units_Base.eq
  let ofName = (name): Units.t => {prefix: Unit, name, power: 1}

  let encode = Encoding.encodeUnits
  let decode = Encoding.decodeUnits

  let compatible = Units_Compatibility.compatible
  let compositeCompatible = Units_Compatibility.compositeCompatible

  let dimensions = Units_Dimensions.ofUnits

  let convert = (value, ~fromUnits, ~toUnits) =>
    Value_Base.toReal(value)->Units_Convert.convert(~fromUnits, ~toUnits)->Value_Base.ofReal
  let convertComposite = (measures, ~toUnits) => {
    let composite = Belt.Array.map(measures, ((value, fromUnits)) => {
      let real = Value_Base.toReal(value)
      (real, fromUnits)
    })->Units_Convert.convertComposite(~toUnits)
    switch composite {
    | Some(composite) =>
      Belt.Array.map(composite, ((real, unitPart)) => {
        let value = Value_Base.ofReal(real)
        (value, unitPart)
      })->Some
    | None => None
    }
  }

  let toMml = (v, ~unitFormat) =>
    Formatting_Measure.formatUnits(~mode=MathML, ~unitFormat=formatUnitFormat(unitFormat), v)
  let toString = Units_Util.toString

  let prefixes = Units_Util.prefixes

  let prefixToString = Units_Util.prefixToString
  let nameToString = Units_Util.nameToString

  let prefixToMml = Units_Util.prefixToMml
}

module Symbol = {
  open TechniCalcEditor

  let eq = Symbol.eq

  let empty = Symbol.empty
  let ofString = Symbol.ofString

  let isEmpty = Symbol.isEmpty
  let isValid = Symbol.isValid

  let encode = Encoding_Symbol.encode
  let decode = string => UrlSafeEncoding.read(string, Encoding_Symbol.read)

  let toMml = Mml_Symbol.toMml
  let ofMml = Mml_Symbol.ofMml

  let alias = Symbol.alias
}

module Graphing = {
  open TechniCalcEditor

  %%private(
    let cmpToString = (v: TechniCalcEditor.AST_Categorization.cmp) => {
      switch v {
      | Cmp_Eq => "eq"
      | Cmp_Gt => "gt"
      | Cmp_Gte => "gte"
      | Cmp_Lt => "lt"
      | Cmp_Lte => "lte"
      }
    }
  )

  let parseAsMetalShaderComponents = elements =>
    Graphing.parseAsMetalShaderComponents(elements)->toJsResult

  let parseAsMetalShader = (~context, elements) => {
    let context = switch context {
    | Some(context) => jsDictEntries(context)
    | None => []
    }
    switch Graphing.parseAsMetalShader(~context, elements) {
    | Ok((eqn, cmp)) => toJsResult(Ok((eqn, cmpToString(cmp))))
    | Error(_) as e => toJsResult(e)
    }
  }
}
