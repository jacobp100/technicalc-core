let matrixOfFloats = (numRows, numColumns, elements) =>
  Belt.Array.map(elements, Scalar.ofFloat)->Matrix.make(~numRows, ~numColumns, _)->Value.ofMatrix
let percentOfFloat = float => Scalar.ofFloat(float)->Value.ofPercent

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

  Formatting.toString(~format, ~inline, x)
}

let ofComplexFloats = (re, im) =>
  Value.add(Value.ofFloat(re), Value.mul(Value.ofFloat(im), Value.i))

let toComplexFloats = (a): (float, float) =>
  switch a {
  | #Zero => (0., 0.)
  | #Real(re) => (Real.toDecimal(re)->Decimal.toFloat, 0.)
  | #Imag(re) => (0., Real.toDecimal(re)->Decimal.toFloat)
  | #Cmpx(re, im) => (Real.toDecimal(re)->Decimal.toFloat, Real.toDecimal(im)->Decimal.toFloat)
  | _ =>
    open Pervasives
    (nan, nan)
  }

%%private(
  let mapMatrixU = (a: Value.t, fn: (. Scalar.t) => 'a): array<array<'a>> => {
    let fn = x => Scalar.Finite.toScalar(x)->fn(. _)

    switch a {
    | #Vect([a, b]) => [[fn(a)], [fn(b)]]
    | #Vect([a, b, c]) => [[fn(a)], [fn(b)], [fn(c)]]
    | #Matx({elements: [a, b, c, d]}) => [[fn(a), fn(b)], [fn(c), fn(d)]]
    | #Matx({elements: [a, b, c, d, e, f, g, h, i]}) => [
        [fn(a), fn(b), fn(c)],
        [fn(d), fn(e), fn(f)],
        [fn(g), fn(h), fn(i)],
      ]
    | _ => assert false
    }
  }
)

let toFloatsMatrix = mapMatrixU(_, (. x) => Scalar.toFloat(x))
let toComplexFloatsMatrix = mapMatrixU(_, (. x) => toComplexFloats(x))
