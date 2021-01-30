[@bs.deriving abstract]
type format = {
  [@bs.optional]
  mode: string,
  [@bs.optional]
  style: string,
  [@bs.optional]
  base: int,
  [@bs.optional]
  precision: int,
  [@bs.optional]
  digitGrouping: bool,
  [@bs.optional]
  decimalMinMagnitude: int,
  [@bs.optional]
  decimalMaxMagnitude: int,
};

module Elements = {
  open TechniCalcEditor;

  let ofString = AST.ofString;

  let encode = Encoding.encode;
  let decode = Encoding.decode;

  let toMml = (x, maybeFormat, maybeInline) => {
    let digitGrouping =
      Belt.Option.flatMap(maybeFormat, digitGroupingGet)
      ->Belt.Option.getWithDefault(true);
    let inline = Belt.Option.getWithDefault(maybeInline, false);
    Mml.create(~digitGrouping, ~inline, x);
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

  let encode = Value_Encoding.encode;
  let decode = Value_Encoding.decode;

  let isNaN = Value_Base.isNaN;

  let ofString = x =>
    switch (Value_Formatting.ofString(x)) {
    | Some(value) => value
    | None => Value_Base.nan
    };

  let toUnicode = (x, maybePrecision) => {
    open Value_Formatting;
    let precision =
      maybePrecision->Belt.Option.getWithDefault(defaultFormat.precision);
    toString(
      ~format={...defaultFormat, mode: Unicode, style: Decimal, precision},
      x,
    );
  };

  let toString = x => Value_Formatting.toString(x);

  let toMml = (x, maybeFormat, maybeInline) => {
    open! Value_Formatting;

    let f = maybeFormat->Belt.Option.getWithDefault(format());

    let inline = Belt.Option.getWithDefault(maybeInline, false);
    let format = {
      mode: MathML,
      style:
        switch (styleGet(f)) {
        | Some("decimal") => Decimal
        | Some("engineering") => Engineering
        | Some("natural-mixed") => Natural({mixedFractions: true})
        | _ => defaultFormat.style
        },
      base: baseGet(f)->Belt.Option.getWithDefault(defaultFormat.base),
      precision:
        precisionGet(f)->Belt.Option.getWithDefault(defaultFormat.precision),
      digitGrouping:
        digitGroupingGet(f)
        ->Belt.Option.getWithDefault(defaultFormat.digitGrouping),
      decimalMinMagnitude:
        decimalMinMagnitudeGet(f)
        ->Belt.Option.getWithDefault(defaultFormat.decimalMinMagnitude),
      decimalMaxMagnitude:
        decimalMaxMagnitudeGet(f)
        ->Belt.Option.getWithDefault(defaultFormat.decimalMaxMagnitude),
    };

    toString(~format, ~inline, x);
  };
};

module Work = {
  let%private encodeContext = context =>
    switch (context) {
    | Some(context) =>
      Js.Dict.entries(context)
      ->Belt.Array.mapU((. (key, value)) => (key, Value.encode(value)))
    | None => [||]
    };

  let calculate = (body, context): Work.t => {
    Calculate(body, encodeContext(context));
  };
  let convertUnits = (body, fromUnits, toUnits, context): Work.t =>
    ConvertUnits({
      body,
      fromUnits,
      toUnits,
      context: encodeContext(context),
    });
  let solveRoot = (lhs, rhs, initialGuess): Work.t =>
    SolveRoot({lhs, rhs, initialGuess});
  let quadratic = (a, b, c): Work.t => Quadratic(a, b, c);
  let cubic = (a, b, c, d): Work.t => Cubic(a, b, c, d);
  let var2 = (x0, y0, c0, x1, y1, c1): Work.t =>
    Var2(x0, y0, c0, x1, y1, c1);
  let var3 = (x0, y0, z0, c0, x1, y1, z1, c1, x2, y2, z2, c2): Work.t =>
    Var3(x0, y0, z0, c0, x1, y1, z1, c1, x2, y2, z2, c2);
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
