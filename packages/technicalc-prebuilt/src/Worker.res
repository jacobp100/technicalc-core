type postMessageData = {
  results: array<string>,
  didError: bool,
}

type onMessageEvent = {data: Work.t}

type self = {
  mutable onmessage: option<onMessageEvent => unit>,
  postMessage: (. postMessageData) => unit,
}

let make = self => {
  open TechniCalcCalculator.AST

  let decodeContext = context =>
    Belt.Array.reduceU(context, TechniCalcCalculator.AST_Context.empty, (. accum, (key, value)) => {
      open TechniCalcCalculator
      Value_Encoding.decode(value)
      ->Belt.Option.getWithDefault(Value_Base.nan)
      ->TechniCalcCalculator.AST_Context.set(accum, key, _)
    })

  let getResults = ({config, context, work}: Work.t) => {
    let context = decodeContext(context)
    switch work {
    | Calculate(body) =>
      let res = eval(~config, ~context, body)
      [res]
    | ConvertUnits({body, fromUnits, toUnits}) =>
      let res =
        eval(~config, ~context, body)->TechniCalcCalculator.Units.convert(~fromUnits, ~toUnits)
      [res]
    | ConvertUnitsComposite({values, toUnits}) =>
      let res =
        Belt.Array.mapU(values, (. (body, unitPart)) => (
          eval(~config, ~context, body),
          unitPart,
        ))->TechniCalcCalculator.Units.convertComposite(_, ~toUnits)
      switch res {
      | Some(res) => res
      | None => Belt.Array.make(Belt.Array.length(values), #N)
      }
    | SolveRoot({body, initialGuess}) =>
      let res = solveRoot(~config, ~context, body, initialGuess)
      [res]
    | Quadratic(a, b, c) =>
      let (x0, x1) = solveQuadratic(~config, ~context, a, b, c)
      [x0, x1]
    | Cubic(a, b, c, d) =>
      let (x0, x1, x2) = solveCubic(~config, ~context, a, b, c, d)
      [x0, x1, x2]
    | Var2(x0, y0, c0, x1, y1, c1) =>
      let (x, y) = solveVar2(~config, ~context, x0, y0, c0, x1, y1, c1)
      [x, y]
    | Var3(x0, y0, z0, c0, x1, y1, z1, c1, x2, y2, z2, c2) =>
      let (x, y, z) = solveVar3(~config, ~context, x0, y0, z0, c0, x1, y1, z1, c1, x2, y2, z2, c2)
      [x, y, z]
    }
  }

  let callback = e => {
    let arg = try {
      results: e.data->getResults->Belt.Array.map(TechniCalcCalculator.Value_Encoding.encode),
      didError: false,
    } catch {
    | _ => {results: [], didError: true}
    }
    self.postMessage(. arg)
  }

  self.onmessage = Some(callback)
}
