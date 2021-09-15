import * as TechniCalc from "../Value/Value";
import cartesian from "./cartesian";
import { Value } from "./index";

const elements = [0, 1, -1, 5];

export const vector2 = cartesian([elements, elements]).map(
  ([a, b]) =>
    new Value(
      [a, b],
      TechniCalc.ofVector([a, b].map(TechniCalc.ofFloat)),
      `[${a}, ${b}]`
    )
);

export const vector3 = cartesian([elements, elements, elements]).map(
  ([a, b, c]) =>
    new Value(
      [a, b, c],
      TechniCalc.ofVector([a, b, c].map(TechniCalc.ofFloat)),
      `[${a}, ${b}, ${c}]`
    )
);
