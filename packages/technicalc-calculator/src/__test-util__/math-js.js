const { range } = require("lodash");
const mathjs = require("mathjs");
const TechniCalc = require("../Value.bs");
const cartesian = require("./cartesian");
const { Value, fractionsTo12 } = require(".");

const trigValues = cartesian([fractionsTo12, fractionsTo12]).map(
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

module.exports.trigValues = trigValues;

const imagValues = cartesian([
  [0.5, 0.25, 0.75, ...range(-5, 5 + 1)],
  [0.5, 0.25, 0.75, ...range(-5, 5 + 1)],
]).map(([re, im]) => Value.complex(re, im));
module.exports.imagValues = imagValues;
