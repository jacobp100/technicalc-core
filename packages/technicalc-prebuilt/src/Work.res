type node = TechniCalcCalculator.AST_Types.t

type context = array<(string, string)>

type work =
  | Calculate(node)
  | ConvertUnits({
      body: node,
      fromUnits: array<TechniCalcCalculator.Unit_Types.unitPart>,
      toUnits: array<TechniCalcCalculator.Unit_Types.unitPart>,
    })
  | ConvertUnitsComposite({
      values: array<(node, TechniCalcCalculator.Unit_Types.unitPart)>,
      toUnits: array<TechniCalcCalculator.Unit_Types.unitPart>,
    })
  | SolveRoot({lhs: node, rhs: node, initialGuess: node})
  | Quadratic(node, node, node)
  | Cubic(node, node, node, node)
  | Var2(node, node, node, node, node, node)
  | Var3(node, node, node, node, node, node, node, node, node, node, node, node)

type t = {
  config: TechniCalcCalculator.AST_Types.config,
  context: context,
  work: work,
}
