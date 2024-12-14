let matrixOfFloats = (numRows, numColumns, elements) =>
  Belt.Array.map(elements, element => Scalar.ofFloat(element))
  ->(Matrix.make(~numRows, ~numColumns, _))
  ->Value.ofMatrix
let percentOfFloat = float => Scalar.ofFloat(float)->Value.ofPercent

@deriving(abstract)
type format = {
  @optional
  mode: string,
  @optional
  style: string,
  @optional
  decimalSeparator: string,
  @optional
  groupingSeparator: string,
  @optional
  base: int,
  @optional
  minDecimalPlaces: int,
  @optional
  maxDecimalPlaces: int,
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
  | _ => (Ascii, false)
  }

  let format = {
    mode,
    style: switch styleGet(f) {
    | Some("decimal") => Decimal
    | Some("engineering") => Engineering
    | Some("natural") => Natural({mixedFractions: false})
    | Some("natural-mixed") => Natural({mixedFractions: true})
    | _ => Natural({mixedFractions: false})
    },
    decimalSeparator: decimalSeparatorGet(f)->Belt.Option.getWithDefault(
      Formatting_Types.defaultFormat.decimalSeparator,
    ),
    groupingSeparator: groupingSeparatorGet(f)->Belt.Option.getWithDefault(
      Formatting_Types.defaultFormat.groupingSeparator,
    ),
    base: baseGet(f)->Belt.Option.getWithDefault(Formatting_Types.defaultFormat.base),
    minDecimalPlaces: minDecimalPlacesGet(f)->Belt.Option.getWithDefault(
      Formatting_Types.defaultFormat.minDecimalPlaces,
    ),
    maxDecimalPlaces: maxDecimalPlacesGet(f)->Belt.Option.getWithDefault(
      Formatting_Types.defaultFormat.maxDecimalPlaces,
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
  let mapMatrix = (a: Value.t, fn: Scalar.t => 'a): array<array<'a>> => {
    let fn = x => Scalar.Finite.toScalar(x)->(fn(_))

    switch a {
    | #Vect([a, b]) => [[fn(a)], [fn(b)]]
    | #Vect([a, b, c]) => [[fn(a)], [fn(b)], [fn(c)]]
    | #Matx({elements: [a, b, c, d]}) => [[fn(a), fn(b)], [fn(c), fn(d)]]
    | #Matx({elements: [a, b, c, d, e, f, g, h, i]}) => [
        [fn(a), fn(b), fn(c)],
        [fn(d), fn(e), fn(f)],
        [fn(g), fn(h), fn(i)],
      ]
    | _ => assert(false)
    }
  }
)

let toFloatsMatrix = mapMatrix(_, x => Scalar.toFloat(x))
let toComplexFloatsMatrix = mapMatrix(_, x => toComplexFloats(x))
