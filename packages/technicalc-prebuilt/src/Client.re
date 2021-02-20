type config = {angleMode: option(string)};

type format = {
  mode: option(string),
  style: option(string),
  locale: option(string),
  base: option(int),
  precision: option(int),
  digitGrouping: option(bool),
  decimalMinMagnitude: option(int),
  decimalMaxMagnitude: option(int),
};

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
};

let%private defaultFormat = TechniCalcCalculator.Formatting_Types.defaultFormat;

[@inline]
let%private formatStyle = f =>
  switch (f.style) {
  | Some("decimal") => TechniCalcCalculator.Value_Formatting.Decimal
  | Some("engineering") => Engineering
  | Some("natural-mixed") => Natural({mixedFractions: true})
  | _ => defaultFormat.style
  };
[@inline]
let%private formatLocale = f =>
  switch (f.locale) {
  | Some("european") => TechniCalcCalculator.Value_Formatting.European
  | _ => defaultFormat.locale
  };
[@inline]
let%private formatBase = f =>
  switch (f.base) {
  | Some(base) => base
  | None => defaultFormat.base
  };
[@inline]
let%private formatPrecision = f =>
  switch (f.precision) {
  | Some(precision) => precision
  | None => defaultFormat.precision
  };
[@inline]
let%private formatDigitGrouping = f =>
  switch (f.digitGrouping) {
  | Some(digitGrouping) => digitGrouping
  | None => defaultFormat.digitGrouping
  };
[@inline]
let%private formatDecimalMinMagnitude = f =>
  switch (f.decimalMinMagnitude) {
  | Some(decimalMinMagnitude) => decimalMinMagnitude
  | None => defaultFormat.decimalMinMagnitude
  };
[@inline]
let%private formatDecimalMaxMagnitude = f =>
  switch (f.decimalMaxMagnitude) {
  | Some(decimalMaxMagnitude) => decimalMaxMagnitude
  | None => defaultFormat.decimalMaxMagnitude
  };

module ElementsMigration = {
  let decode = Client_Migration.TechniCalcEditor.Encoding.decode;
};

module ValueMigration = {
  let decode = encoded =>
    Client_Migration.TechniCalcCalculator.Value_Encoding.decode(encoded)
    ->Belt.Option.getWithDefault(TechniCalcCalculator.Value_Base.nan);
};

module Elements = {
  open TechniCalcEditor;

  let ofString = AST.ofString;

  let encode = Encoding.encode;
  let decode = Encoding.decode;

  let toMml = (x, maybeFormat, maybeInline) => {
    let locale =
      switch (maybeFormat) {
      | Some(format) => formatLocale(format)
      | None => defaultFormat.locale
      };
    let locale =
      switch (locale) {
      | English => Mml.English
      | European => European
      };
    let digitGrouping =
      switch (maybeFormat) {
      | Some(format) => formatDigitGrouping(format)
      | None => defaultFormat.digitGrouping
      };
    let inline =
      switch (maybeInline) {
      | Some(inline) => inline
      | None => false
      };
    Mml.create(~locale, ~digitGrouping, ~inline, x);
  };

  let parse = elements =>
    switch (Value.parse(elements)) {
    | Ok(node) => (None, Some(node))
    | Error(i) => (Some(i), None)
    };

  let bracketRanges = BracketUtil.bracketRanges;
  let bracketRange = BracketUtil.bracketRange;

  let populatedCaptureGroups = CaptureGroupUtil.populatedCaptureGroups;
  let emptyCaptureGroups = CaptureGroupUtil.emptyCaptureGroups;

  let getDependencies = DependencyUtil.getDependencies;

  let inputSettingsMode = InputConfigUtil.inputSettingsMode;

  let insertRanges = InsertUtil.insertRanges;
  let canInsertTable = InsertUtil.canInsertTable;
  let canInsertIteration = InsertUtil.canInsertIteration;
};

module Editor = {
  open TechniCalcEditor;

  let empty = EditState.empty;
  let make = EditState.make;

  let clear = EditState.clear;

  let setIndex = EditState.setIndex;
  let previous = EditState.previous;
  let next = EditState.next;
  let moveStart = EditState.moveStart;
  let moveEnd = EditState.moveEnd;

  let insert = (ast, key) =>
    switch (key) {
    | Keys.One(element) => EditState.insert(ast, element)
    | Many(elements) => EditState.insertArray(ast, elements)
    };
  let delete = EditState.delete;
};

module Keys = {
  open TechniCalcEditor;

  let keys = Keys.keys;

  let variable = (~id, ~name) => Keys.One(VariableS({id, name}));

  let customAtom = (~value, ~mml) =>
    AST.CustomAtomS({
      mml,
      value: TechniCalcCalculator.Value_Encoding.encode(value),
    })
    ->Keys.One;

  let label = (~placeholderMml) =>
    Keys.Many([|
      CaptureGroupStart({placeholderMml: placeholderMml}),
      CaptureGroupEndS,
    |]);

  let equation = (~elements) => Keys.Many(elements);
};

module Value = {
  open TechniCalcCalculator;

  let%private getFormat = (~mode, maybeFormat: option(format)) =>
    switch (maybeFormat) {
    | None => {...Value_Formatting.defaultFormat, mode}
    | Some(format) => {
        mode,
        style: formatStyle(format),
        locale: formatLocale(format),
        base: formatBase(format),
        precision: formatPrecision(format),
        digitGrouping: formatDigitGrouping(format),
        decimalMinMagnitude: formatDecimalMinMagnitude(format),
        decimalMaxMagnitude: formatDecimalMaxMagnitude(format),
      }
    };

  let encode = Value_Encoding.encode;
  let decode = encoded =>
    switch (Value_Encoding.decode(encoded)) {
    | Some(value) => value
    | None => Value_Base.nan
    };

  let isNaN = Value_Base.isNaN;

  let toString = x => Value_Formatting.toString(x);
  let ofString = x =>
    switch (Value_Formatting.ofString(x)) {
    | Some(value) => value
    | None => Value_Base.nan
    };

  let toUnicode = (x, maybeFormat) => {
    let format = getFormat(~mode=Unicode, maybeFormat);
    Value_Formatting.toString(~format, x);
  };

  let toMml = (x, maybeFormat, maybeInline) => {
    let format = getFormat(~mode=MathML, maybeFormat);
    let inline = Belt.Option.getWithDefault(maybeInline, false);
    Value_Formatting.toString(~format, ~inline, x);
  };
};

module Work = {
  let calculate = (body): Work.work => Calculate(body);
  let convertUnits = (body, fromUnits, toUnits): Work.work =>
    ConvertUnits({body, fromUnits, toUnits});
  let solveRoot = (lhs, rhs, initialGuess): Work.work =>
    SolveRoot({lhs, rhs, initialGuess});
  let quadratic = (a, b, c): Work.work => Quadratic(a, b, c);
  let cubic = (a, b, c, d): Work.work => Cubic(a, b, c, d);
  let var2 = (x0, y0, c0, x1, y1, c1): Work.work =>
    Var2(x0, y0, c0, x1, y1, c1);
  let var3 = (x0, y0, z0, c0, x1, y1, z1, c1, x2, y2, z2, c2): Work.work =>
    Var3(x0, y0, z0, c0, x1, y1, z1, c1, x2, y2, z2, c2);

  let make = (config, context, work): Work.t => {
    config: {
      angleMode:
        switch (config.angleMode) {
        | Some("degree") => Degree
        | Some("gradian") => Gradian
        | _ => Radian
        },
    },
    context:
      switch (context) {
      | Some(context) =>
        Js.Dict.entries(context)
        ->Belt.Array.mapU((. (key, value)) => (key, Value.encode(value)))
      | None => [||]
      },
    work,
  };
};

module Units = {
  // let encodeUnitParts = TechniCalcEditor.Encoding_Units.encodeUnitParts;
  // let decodeUnitParts = encoded =>
  //   TechniCalcCalculator.Encoding.read(
  //     encoded,
  //     TechniCalcEditor.Encoding_Units.readUnitParts,
  //   );

  let unitsCompatible = TechniCalcCalculator.Unit_Dimensions.unitsCompatible;

  let toMml = TechniCalcEditor.Mml_Units.toMml;

  let prefixes = Units_Util.prefixes;

  let prefixToString = Units_Util.prefixToString;
  let unitToString = Units_Util.unitToString;
  let unitPartsToString = Units_Util.unitPartsToString;

  let prefixValueToMml = Units_Util.prefixValueToMml;
};
