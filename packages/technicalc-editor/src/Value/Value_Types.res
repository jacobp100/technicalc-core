type node = TechniCalcCalculator.AST_Types.t

type funcitionLike =
  | GenericFunction({fn: AST.fn, resultSuperscript: option<node>})
  | NLog({base: node})
  | Sum({from: node, to_: node})
  | Product({from: node, to_: node})

type partialNode =
  | Resolved(node)
  | Unresolved(AST.foldState<node>, int, int)
  | UnresolvedFunction(funcitionLike, int, int)
