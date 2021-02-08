open TechniCalcCalculator.Unit_Types;

let toUint = (element: unitType): int =>
  Belt.Array.getExn(Encoding_UnitMap_Eval.mapping, Obj.magic(element));
let ofUint = (index: int): option(unitType) =>
  Belt.Array.get(Encoding_UnitMap_Eval.reverseMapping, index);
