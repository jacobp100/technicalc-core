type node = TechniCalcCalculator.AST_Types.t

type partialNode =
  | @as(0) Resolved(node, (int, int))
  | @as(1) Unresolved(AST.foldState<node>, (int, int))
  | @as(2) UnresolvedFunction(AST.fn<node>, option<node>, (int, int))
