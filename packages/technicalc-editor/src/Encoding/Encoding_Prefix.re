open TechniCalcCalculator.Unit_Types;

[@bs.module "./Encoding_PrefixMapping"]
external mapping: array(int) = "mapping";
[@bs.module "./Encoding_PrefixMapping"]
external reverseMapping: array(prefix) = "reverseMapping";

let toUint = (element: prefix): int =>
  Belt.Array.getExn(mapping, Obj.magic(element));
let ofUint = (index: int): option(prefix) =>
  Belt.Array.get(reverseMapping, index);
