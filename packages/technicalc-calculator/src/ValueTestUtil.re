let matrixOfFloats = (numRows, numColumns, elements) =>
  Belt.Array.map(elements, Scalar.ofFloat)
  ->Matrix.make(numRows, numColumns, _)
  ->Value.ofMatrix;
let percentOfFloat = float => Scalar.ofFloat(float)->Value.ofPercent;

let resolve = a => AST.eval(a);
let resolveWithContext = (jsContext, a) => {
  let context =
    Js.Dict.entries(jsContext)
    ->Belt.Array.reduce(Belt.Map.String.empty, (accum, (key, value)) =>
        Belt.Map.String.set(accum, key, value)
      );
  AST.eval(~context, a);
};

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

let toString = (x, maybeFormat) => {
  open Formatting_Types;
  let f = maybeFormat->Belt.Option.getWithDefault(format());

  let (mode, inline) =
    switch (modeGet(f)) {
    | Some("tex") => (Tex, false)
    | Some("mathml") => (MathML, false)
    | Some("mathml-inline") => (MathML, true)
    | _ => (String, false)
    };

  let format = {
    mode,
    style:
      switch (styleGet(f)) {
      | Some("decimal") => Decimal
      | Some("engineering") => Engineering
      | _ => Natural
      },
    base:
      baseGet(f)->Belt.Option.getWithDefault(Formatting_Types.default.base),
    precision:
      precisionGet(f)
      ->Belt.Option.getWithDefault(Formatting_Types.default.precision),
    digitGrouping:
      digitGroupingGet(f)
      ->Belt.Option.getWithDefault(Formatting_Types.default.digitGrouping),
    decimalMinMagnitude:
      decimalMinMagnitudeGet(f)
      ->Belt.Option.getWithDefault(
          Formatting_Types.default.decimalMinMagnitude,
        ),
    decimalMaxMagnitude:
      decimalMaxMagnitudeGet(f)
      ->Belt.Option.getWithDefault(
          Formatting_Types.default.decimalMaxMagnitude,
        ),
  };

  Value.toString(~format, ~inline, x);
};

let ofComplexFloats = (re, im) =>
  Value.add(Value.ofFloat(re), Value.mul(Value.ofFloat(im), Value.i));

let toComplexFloats = (a): (float, float) =>
  switch (a) {
  | `Z => (0., 0.)
  | `R(re) => (Real.toDecimal(re)->Decimal.toFloat, 0.)
  | `I(re) => (0., Real.toDecimal(re)->Decimal.toFloat)
  | `C(re, im) => (
      Real.toDecimal(re)->Decimal.toFloat,
      Real.toDecimal(im)->Decimal.toFloat,
    )
  | _ => Pervasives.(nan, nan)
  };

let%private mapMatrix = (a: Value.t, fn: Scalar.t => 'a): array(array('a)) => {
  switch (a) {
  | `V([|a, b|]) => [|[|fn(a)|], [|fn(b)|]|]
  | `V([|a, b, c|]) => [|[|fn(a)|], [|fn(b)|], [|fn(c)|]|]
  | `M({elements: [|a, b, c, d|]}) => [|
      [|fn(a), fn(b)|],
      [|fn(c), fn(d)|],
    |]
  | `M({elements: [|a, b, c, d, e, f, g, h, i|]}) => [|
      [|fn(a), fn(b), fn(c)|],
      [|fn(d), fn(e), fn(f)|],
      [|fn(g), fn(h), fn(i)|],
    |]
  | _ => invalid_arg("Not a matrix")
  };
};

let toFloatsMatrix = mapMatrix(_, Scalar.toFloat);
let toComplexFloatsMatrix = mapMatrix(_, toComplexFloats);

let testUnits = Js.Dict.empty();
Js.Dict.set(testUnits, "second", Unit_Types.Second);
Js.Dict.set(testUnits, "hour", Unit_Types.Hour);
Js.Dict.set(testUnits, "meter", Unit_Types.Meter);
Js.Dict.set(testUnits, "inch", Unit_Types.Inch);
Js.Dict.set(testUnits, "acre", Unit_Types.Acre);
Js.Dict.set(testUnits, "liter", Unit_Types.Liter);
