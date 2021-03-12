module AST = {
  let ofInt = x => AST_Types.OfInt(x)
  let add = (x, y) => AST_Types.Add(x, y)
  let sub = (x, y) => AST_Types.Sub(x, y)
  let mul = (x, y) => AST_Types.Mul(x, y)
  let div = (x, y) => AST_Types.Div(x, y)
  let pow = (x, y) => AST_Types.Pow(x, y)
  let sin = x => AST_Types.Sin(x)
  let variable = x => AST_Types.Variable(x)
}

let matrixOfFloats = (numRows, numColumns, elements) =>
  Belt.Array.map(elements, Scalar.ofFloat)->Matrix.make(numRows, numColumns, _)->Value.ofMatrix
let percentOfFloat = float => Scalar.ofFloat(float)->Value.ofPercent

let resolve = a => AST_Evaluation.eval(a)
let resolveWithContext = (jsContext, a) => {
  let context =
    Js.Dict.entries(jsContext)->Belt.Array.reduceU(AST_Context.empty, (. accum, (key, value)) =>
      AST_Context.set(accum, key, value)
    )
  AST_Evaluation.eval(~context, a)
}

@deriving(abstract)
type format = {
  @optional
  mode: string,
  @optional
  style: string,
  @optional
  locale: string,
  @optional
  base: int,
  @optional
  precision: int,
  @optional
  digitGrouping: bool,
  @optional
  decimalMinMagnitude: int,
  @optional
  decimalMaxMagnitude: int,
}

let toString = (x, maybeFormat) => {
  open Formatting_Types
  let f = maybeFormat->Belt.Option.getWithDefault(format())

  let (mode, inline) = switch modeGet(f) {
  | Some("tex") => (Tex, false)
  | Some("mathml") => (MathML, false)
  | Some("mathml-inline") => (MathML, true)
  | _ => (String, false)
  }

  let format = {
    mode: mode,
    style: switch styleGet(f) {
    | Some("decimal") => Decimal
    | Some("engineering") => Engineering
    | Some("natural-mixed") => Natural({mixedFractions: true})
    | _ => defaultFormat.style
    },
    locale: switch localeGet(f) {
    | Some("european") => European
    | _ => defaultFormat.locale
    },
    base: baseGet(f)->Belt.Option.getWithDefault(Formatting_Types.defaultFormat.base),
    precision: precisionGet(f)->Belt.Option.getWithDefault(
      Formatting_Types.defaultFormat.precision,
    ),
    digitGrouping: digitGroupingGet(f)->Belt.Option.getWithDefault(
      Formatting_Types.defaultFormat.digitGrouping,
    ),
    decimalMinMagnitude: decimalMinMagnitudeGet(f)->Belt.Option.getWithDefault(
      Formatting_Types.defaultFormat.decimalMinMagnitude,
    ),
    decimalMaxMagnitude: decimalMaxMagnitudeGet(f)->Belt.Option.getWithDefault(
      Formatting_Types.defaultFormat.decimalMaxMagnitude,
    ),
  }

  Value.toString(~format, ~inline, x)
}

let ofComplexFloats = (re, im) =>
  Value.add(Value.ofFloat(re), Value.mul(Value.ofFloat(im), Value.i))

let toComplexFloats = (a): (float, float) =>
  switch a {
  | #Z => (0., 0.)
  | #R(re) => (Real.toDecimal(re)->Decimal.toFloat, 0.)
  | #I(re) => (0., Real.toDecimal(re)->Decimal.toFloat)
  | #C(re, im) => (Real.toDecimal(re)->Decimal.toFloat, Real.toDecimal(im)->Decimal.toFloat)
  | _ =>
    open Pervasives
    (nan, nan)
  }

%%private(
  let mapMatrix = (a: Value.t, fn: Scalar.t => 'a): array<array<'a>> =>
    switch a {
    | #V([a, b]) => [[fn(a)], [fn(b)]]
    | #V([a, b, c]) => [[fn(a)], [fn(b)], [fn(c)]]
    | #M({elements: [a, b, c, d]}) => [[fn(a), fn(b)], [fn(c), fn(d)]]
    | #M({elements: [a, b, c, d, e, f, g, h, i]}) => [
        [fn(a), fn(b), fn(c)],
        [fn(d), fn(e), fn(f)],
        [fn(g), fn(h), fn(i)],
      ]
    | _ => assert false
    }
)

let toFloatsMatrix = mapMatrix(_, Scalar.toFloat)
let toComplexFloatsMatrix = mapMatrix(_, toComplexFloats)

let testPrefixes = Js.Dict.empty()
Js.Dict.set(testPrefixes, "unit", Unit_Types.Unit)
Js.Dict.set(testPrefixes, "milli", Unit_Types.Milli)
Js.Dict.set(testPrefixes, "kilo", Unit_Types.Kilo)

let testUnits = Js.Dict.empty()
Js.Dict.set(testUnits, "second", Unit_Types.Second)
Js.Dict.set(testUnits, "hour", Unit_Types.Hour)
Js.Dict.set(testUnits, "meter", Unit_Types.Meter)
Js.Dict.set(testUnits, "inch", Unit_Types.Inch)
Js.Dict.set(testUnits, "acre", Unit_Types.Acre)
Js.Dict.set(testUnits, "liter", Unit_Types.Liter)
