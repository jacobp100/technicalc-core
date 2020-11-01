open TechniCalcCalculator.Unit_Types;

[@bs.module "./Encoding_ElementMapping"]
external mapping: array(int) = "mapping";
[@bs.module "./Encoding_ElementMapping"]
external reverseMapping: array(unitType) = "reverseMapping";

let toInt = (element: unitType): int =>
  Belt.Array.getExn(mapping, Obj.magic(element));
let ofInt = (index: int): option(unitType) =>
  Belt.Array.get(reverseMapping, index);
