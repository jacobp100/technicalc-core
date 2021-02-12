import _ from "lodash";
import * as mathjs from "mathjs/lib/esm";
import * as TechniCalc from "../Value";
import cartesian from "./_cartesian";
import { Value, fractionsTo12 } from "./_index";

export const trigValues = cartesian([fractionsTo12, fractionsTo12]).map(
  ([[numRe, denomRe], [numIm, denomIm]]) =>
    new Value(
      mathjs.complex((Math.PI * numRe) / denomRe, (Math.PI * numIm) / denomIm),
      TechniCalc.mul(
        TechniCalc.add(
          TechniCalc.div(
            TechniCalc.ofFloat(numRe),
            TechniCalc.ofFloat(denomRe)
          ),
          TechniCalc.mul(
            TechniCalc.div(
              TechniCalc.ofFloat(numIm),
              TechniCalc.ofFloat(denomIm)
            ),
            TechniCalc.i
          )
        ),
        TechniCalc.pi
      ),
      `(${numRe}/${denomRe}+${numIm}i/${denomIm})pi`
    )
);

export const complexValues = cartesian([
  [0.5, 0.25, 0.75, ..._.range(-5, 5 + 1)],
  [0.5, 0.25, 0.75, ..._.range(-5, 5 + 1)],
]).map(([re, im]) => Value.complex(re, im));
