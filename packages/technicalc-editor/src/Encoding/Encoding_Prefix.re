open TechniCalcCalculator.Unit_Types;

let toUint = (element: prefix): int =>
  Belt.Array.getExn(Encoding_PrefixMap_Eval.mapping, Obj.magic(element));
let ofUint = (index: int): option(prefix) =>
  Belt.Array.get(Encoding_PrefixMap_Eval.reverseMapping, index);
