import * as TechniCalc from "../Value/Value";
import cartesian from "./cartesian";
import { Value, range12, fractionsTo12 } from "./index";

const range12Values = range12.map(Value.float);
const range12NegativeValues = range12.map((x) => Value.float(-x));

const trigPositiveValues = fractionsTo12.map(
  ([num, denom]) =>
    new Value(
      (Math.PI * num) / denom,
      TechniCalc.mul(
        TechniCalc.div(TechniCalc.ofFloat(num), TechniCalc.ofFloat(denom)),
        TechniCalc.pi
      ),
      `${num}pi/${denom}`
    )
);

const trigNegativeValues = fractionsTo12.map(
  ([num, denom]) =>
    new Value(
      -(Math.PI * num) / denom,
      TechniCalc.mul(
        TechniCalc.div(TechniCalc.ofFloat(-num), TechniCalc.ofFloat(denom)),
        TechniCalc.pi
      ),
      `${-num}pi/${denom}`
    )
);

const positiveValues = [...range12Values, ...trigPositiveValues];

const values = [...positiveValues, ...range12NegativeValues];

export const binaryValues = cartesian([values, values]);

export const positiveBinaryValues = cartesian([positiveValues, positiveValues]);

export const trigValues = [...trigPositiveValues, ...trigNegativeValues];
