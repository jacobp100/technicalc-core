type onMessageEvent<'output> = {data: Work.t<'output>}

type self<'output> = {
  mutable onmessage: option<onMessageEvent<'output> => unit>,
  postMessage: (. string) => unit,
}

%%private(
  let decodeContext = context =>
    Belt.Array.reduceU(context, TechniCalcCalculator.AST_Context.empty, (. accum, (key, value)) => {
      open TechniCalcCalculator
      Encoding.decode(value)
      ->Belt.Option.getWithDefault(Value_Base.nan)
      ->TechniCalcCalculator.AST_Context.set(accum, key, _)
    })
)

%%private(
  let compute = (type output, {config, context, input}: Work.t<output>): output => {
    open TechniCalcCalculator.AST

    let context = decodeContext(context)
    switch input {
    | Work.Calculate(body) => (eval(~config, ~context, body): TechniCalcCalculator.Value.t)
    | Work.ConvertUnits({body, fromUnits, toUnits}) =>
      let value =
        eval(~config, ~context, body)->TechniCalcCalculator.Units.convert(~fromUnits, ~toUnits)
      [(value, toUnits)]
    | Work.ConvertUnitsComposite({values, toUnits}) =>
      let res =
        Belt.Array.mapU(values, (. (body, unitPart)) => (
          eval(~config, ~context, body),
          unitPart,
        ))->TechniCalcCalculator.Units.convertComposite(_, ~toUnits)
      switch res {
      | Some(res) => Belt.Array.mapU(res, (. (value, unitPart)) => (value, [unitPart]))
      | None =>
        let unitsSorted = TechniCalcCalculator.Units.compositeUnitsSorted(toUnits)
        Belt.Array.mapU(unitsSorted, (. unitPart) => (#NaNN, [unitPart]))
      }
    | Work.SolveRoot({body, initialGuess}) => solveRoot(~config, ~context, body, initialGuess)
    | Work.Quadratic(a, b, c) => solveQuadratic(~config, ~context, a, b, c)
    | Work.Cubic(a, b, c, d) => solveCubic(~config, ~context, a, b, c, d)
    | Work.Var2(x0, y0, c0, x1, y1, c1) => solveVar2(~config, ~context, x0, y0, c0, x1, y1, c1)
    | Work.Var3(x0, y0, z0, c0, x1, y1, z1, c1, x2, y2, z2, c2) =>
      solveVar3(~config, ~context, x0, y0, z0, c0, x1, y1, z1, c1, x2, y2, z2, c2)
    }
  }
)

let make = self => {
  let callback = e => {
    let work = e.data
    let res = compute(work)
    let encoded = Work.encodeOutput(work, res)
    self.postMessage(. encoded)
  }

  self.onmessage = Some(callback)
}
