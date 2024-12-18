type node = TechniCalcCalculator.AST_Types.t
type value = TechniCalcCalculator.Value.t
type units = array<TechniCalcCalculator.Units_Types.t>
type unitsResult = array<(value, units)>
type currency = (float, string)
type currencyResult = array<(value, string)>

type contextEntry = (string, value)
type context = array<contextEntry>

type rec input<'output> =
  | Calculate(node): input<value>
  | ConvertUnits({
      body: node,
      fromUnits: array<TechniCalcCalculator.Units_Types.t>,
      toUnits: array<TechniCalcCalculator.Units_Types.t>,
    }): input<unitsResult>
  | ConvertUnitsComposite({
      values: array<(node, TechniCalcCalculator.Units_Types.t)>,
      toUnits: array<TechniCalcCalculator.Units_Types.t>,
    }): input<unitsResult>
  | ConvertCurrencies({
      body: node,
      fromCurrency: currency,
      toCurrencies: array<currency>,
    }): input<currencyResult>
  | SolveRoot({body: node, initialGuess: node}): input<value>
  | Quadratic(node, node, node): input<(value, value)>
  | Cubic(node, node, node, node): input<(value, value, value)>
  | Var2(node, node, node, node, node, node): input<(value, value)>
  | Var3(node, node, node, node, node, node, node, node, node, node, node, node): input<(
      value,
      value,
      value,
    )>

type t<'output> = {
  config: TechniCalcCalculator.AST_Types.config,
  context: context,
  input: input<'output>,
}

%%private(
  let encodeContextElement = ((key, value): contextEntry) => (
    key,
    TechniCalcCalculator.Encoding.encode(value),
  )
)
%%private(
  let decodeContextElement = ((key, value)) => (
    key,
    TechniCalcCalculator.Encoding.decode(value)->Belt.Option.getWithDefault(
      TechniCalcCalculator.Value_Base.nan,
    ),
  )
)

%%private(let encodeContext = context => Belt.Array.map(context, encodeContextElement))

%%private(let decodeContext = encoding => Belt.Array.map(encoding, decodeContextElement))

let encodeInput = (work: t<node>) =>
  Obj.magic({
    "config": work.config,
    "context": encodeContext(work.context),
    "input": work.input,
  })->Js_json.stringify

let decodeInput = (encoded: string): option<t<node>> =>
  try {
    let encoded = Obj.magic(Js_json.parseExn(encoded))
    let config = encoded["config"]
    let context = decodeContext(encoded["context"])
    let input = encoded["input"]
    Some({config, context, input})
  } catch {
  | _ => None
  }

%%private(
  let encodeUnit = ({prefix, name, power}: TechniCalcCalculator.Units_Types.t) => {
    UrlSafeEncoding.encodeInt(Obj.magic(prefix)) ++
    UrlSafeEncoding.encodeInt(Obj.magic(name)) ++
    UrlSafeEncoding.encodeInt(power)
  }
)
%%private(
  let readUnit = (reader): option<TechniCalcCalculator.Units_Types.t> =>
    switch (
      UrlSafeEncoding.readInt(reader),
      UrlSafeEncoding.readInt(reader),
      UrlSafeEncoding.readInt(reader),
    ) {
    | (Some(prefix), Some(name), Some(power)) =>
      Some({
        prefix: Obj.magic(prefix),
        name: Obj.magic(name),
        power,
      })
    | _ => None
    }
)

%%private(let encodeUnits = units => UrlSafeEncoding.encodeArray(units, encodeUnit))
%%private(let readUnits = reader => UrlSafeEncoding.readArray(reader, readUnit))

%%private(
  let encodeValueUnitTuple = ((value, units)) =>
    TechniCalcCalculator.Encoding.encode(value) ++ encodeUnits(units)
)
%%private(
  let readValueUnitTuple = reader =>
    switch (TechniCalcCalculator.Encoding_Value.read(reader), readUnits(reader)) {
    | (Some(value), Some(units)) => Some((value, units))
    | _ => None
    }
)

%%private(
  let encodeUnitsResult = (unitsResult: unitsResult) =>
    UrlSafeEncoding.encodeArray(unitsResult, encodeValueUnitTuple)
)
%%private(
  let readUnitsResult = reader => {
    UrlSafeEncoding.readArray(reader, readValueUnitTuple)
  }
)
%%private(
  let decodeUnitsResult = (encoded: string) => UrlSafeEncoding.read(encoded, readUnitsResult)
)

%%private(
  let encodeValueCurrencyTuple = ((value, currency)) =>
    TechniCalcCalculator.Encoding.encode(value) ++ UrlSafeEncoding.encodeString(currency)
)
%%private(
  let readValueCurrencyTuple = reader =>
    switch (TechniCalcCalculator.Encoding_Value.read(reader), UrlSafeEncoding.readString(reader)) {
    | (Some(value), Some(currency)) => Some((value, currency))
    | _ => None
    }
)

%%private(
  let encodeCurrenciesResult = (currencyResult: currencyResult) =>
    UrlSafeEncoding.encodeArray(currencyResult, encodeValueCurrencyTuple)
)
%%private(
  let readCurrenciesResult = reader => {
    UrlSafeEncoding.readArray(reader, readValueCurrencyTuple)
  }
)
%%private(
  let decodeCurrenciesResult = (encoded: string) =>
    UrlSafeEncoding.read(encoded, readCurrenciesResult)
)

%%private(let encodeValue = TechniCalcCalculator.Encoding.encode)
%%private(let decodeValue = TechniCalcCalculator.Encoding.decode)

%%private(
  let encode2Values = ((v0, v1)) =>
    TechniCalcCalculator.Encoding.encode(v0) ++ TechniCalcCalculator.Encoding.encode(v1)
)
%%private(
  let decode2Values = encoding =>
    UrlSafeEncoding.read(encoding, reader => {
      switch (
        TechniCalcCalculator.Encoding_Value.read(reader),
        TechniCalcCalculator.Encoding_Value.read(reader),
      ) {
      | (Some(v0), Some(v1)) => Some((v0, v1))
      | _ => None
      }
    })
)

%%private(
  let encode3Values = ((v0, v1, v2)) =>
    TechniCalcCalculator.Encoding.encode(v0) ++
    TechniCalcCalculator.Encoding.encode(v1) ++
    TechniCalcCalculator.Encoding.encode(v2)
)
%%private(
  let decode3Values = encoding =>
    UrlSafeEncoding.read(encoding, reader => {
      switch (
        TechniCalcCalculator.Encoding_Value.read(reader),
        TechniCalcCalculator.Encoding_Value.read(reader),
        TechniCalcCalculator.Encoding_Value.read(reader),
      ) {
      | (Some(v0), Some(v1), Some(v2)) => Some((v0, v1, v2))
      | _ => None
      }
    })
)

let encodeOutput = (type output, work: t<output>, output: output): string => {
  switch work.input {
  | Calculate(_) => encodeValue(output)
  | ConvertUnits(_) => encodeUnitsResult(output)
  | ConvertUnitsComposite(_) => encodeUnitsResult(output)
  | ConvertCurrencies(_) => encodeCurrenciesResult(output)
  | SolveRoot(_) => encodeValue(output)
  | Quadratic(_) => encode2Values(output)
  | Cubic(_) => encode3Values(output)
  | Var2(_) => encode2Values(output)
  | Var3(_) => encode3Values(output)
  }
}

let decodeOutput = (type output, work: t<output>, encoded: string): option<output> => {
  switch work.input {
  | Calculate(_) => decodeValue(encoded)
  | ConvertUnits(_) => decodeUnitsResult(encoded)
  | ConvertUnitsComposite(_) => decodeUnitsResult(encoded)
  | ConvertCurrencies(_) => decodeCurrenciesResult(encoded)
  | SolveRoot(_) => decodeValue(encoded)
  | Quadratic(_) => decode2Values(encoded)
  | Cubic(_) => decode3Values(encoded)
  | Var2(_) => decode2Values(encoded)
  | Var3(_) => decode3Values(encoded)
  }
}
