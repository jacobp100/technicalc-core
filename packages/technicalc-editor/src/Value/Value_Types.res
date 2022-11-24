type node = TechniCalcCalculator.AST_Types.t

type funcitionLike =
  | GenericFunction({fn: AST.fn, resultSuperscript: option<node>})
  | NLog({base: node})
  | Sum({from: node, to: node})
  | Product({from: node, to: node})

type partialNode =
  | Resolved(node, (int, int))
  | Unresolved(AST.foldState<node>, (int, int))
  | UnresolvedFunction(funcitionLike, (int, int))
