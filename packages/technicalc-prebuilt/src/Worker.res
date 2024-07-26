type onMessageEvent = {data: string}

type self = {
  mutable onmessage: onMessageEvent => unit,
  postMessage: (. string) => unit,
}

@val external self: self = "self"

%%private(
  let decodeContext = context =>
    Belt.Array.reduceU(context, TechniCalcCalculator.AST_Context.empty, (. accum, (key, value)) => {
      TechniCalcCalculator.AST_Context.set(accum, key, value)
    })
)

%%private(
  let compute = (type output, {config, context, input}: Work.t<output>): output => {
    open TechniCalcCalculator.AST

    TechniCalcCalculator.Decimal.resetInternalState()

    let context = decodeContext(context)
    switch input {
    | Work.Calculate(body) => (eval(~config, ~context, body): TechniCalcCalculator.Value.t)
    | Work.ConvertUnits({body, fromUnits, toUnits}) =>
      let value =
        eval(~config, ~context, body)
        ->TechniCalcCalculator.Value.toReal
        ->TechniCalcCalculator.Units.convert(~fromUnits, ~toUnits)
        ->TechniCalcCalculator.Value.ofReal

      [(value, toUnits)]
    | Work.ConvertUnitsComposite({values, toUnits}) =>
      let res =
        Belt.Array.mapU(values, (. (body, unitPart)) => (
          eval(~config, ~context, body)->TechniCalcCalculator.Value.toReal,
          unitPart,
        ))->TechniCalcCalculator.Units.convertComposite(_, ~toUnits)
      switch res {
      | Some(res) =>
        Belt.Array.mapU(res, (. (value, unitPart)) => (
          TechniCalcCalculator.Value.ofReal(value),
          [unitPart],
        ))
      | None =>
        // TODO - sort units (NB - this would have crashed on stuff like Celsius)
        // let unitsSorted = TechniCalcCalculator.Units.compositeUnitsSorted(toUnits)
        Belt.Array.mapU(toUnits, (. unitPart) => (#NaNN, [unitPart]))
      }
    | Work.ConvertCurrencies({body, fromCurrency: (fromCurrencyValue, _), toCurrencies}) =>
      let value = eval(~config, ~context, body)
      let fromCurrency = TechniCalcCalculator.Value.ofFloat(fromCurrencyValue)
      Belt.Array.mapU(toCurrencies, (. (toCurrencyValue, toCurrencyName)) => {
        let toCurrency = TechniCalcCalculator.Value.ofFloat(toCurrencyValue)
        let value = TechniCalcCalculator.Value.div(
          TechniCalcCalculator.Value.mul(value, toCurrency),
          fromCurrency,
        )
        (value, toCurrencyName)
      })
    | Work.SolveRoot({body, initialGuess}) => solveRoot(~config, ~context, body, initialGuess)
    | Work.Quadratic(a, b, c) => solveQuadratic(~config, ~context, a, b, c)
    | Work.Cubic(a, b, c, d) => solveCubic(~config, ~context, a, b, c, d)
    | Work.Var2(x0, y0, c0, x1, y1, c1) => solveVar2(~config, ~context, x0, y0, c0, x1, y1, c1)
    | Work.Var3(x0, y0, z0, c0, x1, y1, z1, c1, x2, y2, z2, c2) =>
      solveVar3(~config, ~context, x0, y0, z0, c0, x1, y1, z1, c1, x2, y2, z2, c2)
    }
  }
)

self.onmessage = e => {
  switch Work.decodeInput(e.data) {
  | Some(work) =>
    let res = compute(work)
    let encoded = Work.encodeOutput(work, res)
    self.postMessage(. encoded)
  | None => ()
  }
}
