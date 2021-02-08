open AST;

let toUint = (element: t): int =>
  Belt.Array.getExn(Encoding_ElementMap_Eval.mapping, Obj.magic(element));
let ofUint = (index: int): option(t) =>
  Belt.Array.get(Encoding_ElementMap_Eval.reverseMapping, index);
