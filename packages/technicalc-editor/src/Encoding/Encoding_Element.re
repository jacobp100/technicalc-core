open AST;

[@bs.module "./Encoding_ElementMapping"]
external mapping: array(int) = "mapping";
[@bs.module "./Encoding_ElementMapping"]
external reverseMapping: array(t) = "reverseMapping";

let toInt = (element: t): int =>
  Belt.Array.getExn(mapping, Obj.magic(element));
let ofInt = (index: int): option(t) =>
  Belt.Array.get(reverseMapping, index);
