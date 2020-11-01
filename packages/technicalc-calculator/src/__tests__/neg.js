const { toMatchJsValue } = require("../__test-util__");
const TechniCalc = require("../Value.bs");

expect.extend({ toMatchJsValue });

test("-1", () => {
  expect(TechniCalc.neg(TechniCalc.one)).toMatchJsValue(-1);
});

test("-pi (float value)", () => {
  expect(TechniCalc.neg(TechniCalc.ofFloat(Math.PI))).toMatchJsValue(-Math.PI);
});
