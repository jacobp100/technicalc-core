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

  let encode = Encoding.encode;
  let decode = Encoding.decode;

  let ofValue = AST.ofValue;

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

  let label = (~mml) => AST.LabelS({mml: mml})->Keys.One;

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
      maybePrecision->Belt.Option.getWithDefault(default.precision);
    toString(
      ~format={...default, mode: Unicode, style: Decimal, precision},
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
        | _ => Natural
        },
      base: baseGet(f)->Belt.Option.getWithDefault(default.base),
      precision:
        precisionGet(f)->Belt.Option.getWithDefault(default.precision),
      digitGrouping:
        digitGroupingGet(f)
        ->Belt.Option.getWithDefault(default.digitGrouping),
      decimalMinMagnitude:
        decimalMinMagnitudeGet(f)
        ->Belt.Option.getWithDefault(default.decimalMinMagnitude),
      decimalMaxMagnitude:
        decimalMaxMagnitudeGet(f)
        ->Belt.Option.getWithDefault(default.decimalMaxMagnitude),
    };

    toString(~format, ~inline, x);
  };
};

module Work = {
  [@bs.scope "Object"] [@bs.val]
  external entries: Js.Dict.t('a) => array((string, 'a)) = "entries";

  let calculate =
      (body, context: option(Js.Dict.t(TechniCalcCalculator.Value_Types.t)))
      : Work.t => {
    let context =
      switch (context) {
      | Some(context) =>
        entries(context)
        ->Belt.Array.mapU((. (key, value)) => (key, Value.encode(value)))
      | None => [||]
      };
    Calculate(body, context);
  };
  let convertUnits = (body, fromUnits, toUnits): Work.t =>
    ConvertUnits(body, fromUnits, toUnits);
  let solveRoot = (body, initial): Work.t => SolveRoot(body, initial);
  let quadratic = (a, b, c): Work.t => Quadratic(a, b, c);
  let cubic = (a, b, c, d): Work.t => Cubic(a, b, c, d);
  let var2 = (x0, y0, c0, x1, y1, c1): Work.t =>
    Var2(x0, y0, c0, x1, y1, c1);
  let var3 = (x0, y0, z0, c0, x1, y1, z1, c1, x2, y2, z2, c2): Work.t =>
    Var3(x0, y0, z0, c0, x1, y1, z1, c1, x2, y2, z2, c2);
};

module Units = {
  let encodeUnitPowers = TechniCalcEditor.Encoding_Units.encodeUnitPowers;
  let decodeUnitPowers = encoded =>
    TechniCalcCalculator.Encoding.read(
      encoded,
      TechniCalcEditor.Encoding_Units.readUnitPowers,
    );

  let unitsCompatible = TechniCalcCalculator.Unit_Dimensions.unitsCompatible;

  let unitToMml = TechniCalcEditor.Mml_Units.unitToMml;
  let unitPowerToMml = TechniCalcEditor.Mml_Units.unitPowerToMml;
  let unitPowersToMml = TechniCalcEditor.Mml_Units.unitPowersToMml;

  let units = Units.units;
};
