type node = TechniCalcCalculator.AST_Types.t;

type context = array((string, string));

type work =
  | Calculate(node, context)
  | ConvertUnits({
      body: node,
      fromUnits: array(TechniCalcCalculator.Unit_Types.unitPart),
      toUnits: array(TechniCalcCalculator.Unit_Types.unitPart),
      context,
    })
  | SolveRoot({
      lhs: node,
      rhs: node,
      initialGuess: node,
    })
  | Quadratic(node, node, node)
  | Cubic(node, node, node, node)
  | Var2(node, node, node, node, node, node)
  | Var3(
      node,
      node,
      node,
      node,
      node,
      node,
      node,
      node,
      node,
      node,
      node,
      node,
    );

type t = {
  config: TechniCalcCalculator.AST_Types.config,
  work,
};
