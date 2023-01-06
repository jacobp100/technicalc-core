type node = TechniCalcCalculator.AST_Types.t

type partialNode =
  | Resolved(node, (int, int))
  | Unresolved(AST.foldState<node>, (int, int))
  | UnresolvedFunction(AST.fn<node>, option<node>, (int, int))
