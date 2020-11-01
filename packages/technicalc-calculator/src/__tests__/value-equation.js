const { range } = require("lodash");
const TechniCalc = require("../Value.bs");
const cartesian = require("../__test-util__/cartesian");

const testValues = range(-2, 2 + 1, 1);

const quadraticValues = cartesian([testValues, testValues]);

test("quadratic", () => {
  // (x - %s)(x - %s) = 0
  quadraticValues.forEach(([x0, x1]) => {
    const a = TechniCalc.ofFloat(1);
    const b = TechniCalc.ofFloat(-(x0 + x1));
    const c = TechniCalc.ofFloat(x0 * x1);
    const [r0, r1] = TechniCalc.quadratic(a, b, c);
    expect([TechniCalc.toFloat(r0), TechniCalc.toFloat(r1)].sort()).toEqual(
      [x0, x1].sort()
    );
  });
});

const cubicValues = cartesian([testValues, testValues, testValues]);

test("cubic", () => {
  // (x - %s)(x - %s)(x - %s) = 0
  cubicValues.forEach(([x0, x1, x2]) => {
    const a = TechniCalc.ofFloat(1);
    const b = TechniCalc.ofFloat(-(x0 + x1 + x2));
    const c = TechniCalc.ofFloat(x0 * x1 + x0 * x2 + x1 * x2);
    const d = TechniCalc.ofFloat(-x0 * x1 * x2);
    const [r0, r1, r2] = TechniCalc.cubic(a, b, c, d);
    expect(
      [
        TechniCalc.toFloat(r0),
        TechniCalc.toFloat(r1),
        TechniCalc.toFloat(r2),
      ].sort()
    ).toEqual([x0, x1, x2].sort());
  });
});
