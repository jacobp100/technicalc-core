const TechniCalc = require("../Value.bs");
const cartesian = require("./cartesian");
const { Value } = require(".");

const elements = [0, 1, -1, 5];

const vector2 = cartesian([elements, elements]).map(
  ([a, b]) =>
    new Value(
      [a, b],
      TechniCalc.ofVector([a, b].map(TechniCalc.ofFloat)),
      `[${a}, ${b}]`
    )
);
module.exports.vector2 = vector2;

const vector3 = cartesian([elements, elements, elements]).map(
  ([a, b, c]) =>
    new Value(
      [a, b, c],
      TechniCalc.ofVector([a, b, c].map(TechniCalc.ofFloat)),
      `[${a}, ${b}, ${c}]`
    )
);
module.exports.vector3 = vector3;
