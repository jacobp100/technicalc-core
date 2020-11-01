const mathjs = require("mathjs");
const TechniCalc = require("../Value.bs");
const { toMatchJsMatrix } = require("../__test-util__");
const { vector3 } = require("../__test-util__/math-js-vector");
const cartesian = require("../__test-util__/cartesian");

expect.extend({ toMatchJsMatrix });

cartesian([vector3, vector3]).forEach(([a, b]) => {
  it(`${a.title} x ${b.title}`, () => {
    const [i, j, k] = mathjs.cross(a.jsValue, b.jsValue);
    const mathJsValue = mathjs.matrix([[i], [j], [k]]);
    expect(
      TechniCalc.mul(a.techniCalcValue, b.techniCalcValue)
    ).toMatchJsMatrix(mathJsValue);
  });
});
