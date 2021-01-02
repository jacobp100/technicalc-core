open TechniCalcCalculator.Unit_Types;

[@bs.module "./Encoding_UnitMapping"]
external mapping: array(int) = "mapping";
[@bs.module "./Encoding_UnitMapping"]
external reverseMapping: array(unitType) = "reverseMapping";

let toUint = (element: unitType): int =>
  Belt.Array.getExn(mapping, Obj.magic(element));
let ofUint = (index: int): option(unitType) =>
  Belt.Array.get(reverseMapping, index);
