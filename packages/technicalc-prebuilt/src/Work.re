type node = TechniCalcCalculator.AST_Types.t;

type context = array((string, string));

type t =
  | Calculate(node, context)
  | ConvertUnits(
      node,
      array(TechniCalcCalculator.Unit_Types.unitPart),
      array(TechniCalcCalculator.Unit_Types.unitPart),
      context,
    )
  | SolveRoot(node, node)
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
