open AST;

[@bs.module "./LegacyEncoding_ElementMapping"]
external mapping: array(int) = "mapping";
[@bs.module "./LegacyEncoding_ElementMapping"]
external reverseMapping: array(t) = "reverseMapping";

let toUint = (element: t): int =>
  Belt.Array.getExn(mapping, Obj.magic(element));
let ofUint = (index: int): option(t) =>
  Belt.Array.get(reverseMapping, index);