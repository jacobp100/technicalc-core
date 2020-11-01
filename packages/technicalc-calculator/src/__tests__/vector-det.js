const { toMatchJsValue } = require("../__test-util__");
const TechniCalc = require("../Value.bs");

expect.extend({ toMatchJsValue });

it(`det [3 ,7, 8]`, () => {
  expect(
    TechniCalc.abs(TechniCalc.ofVector([3, 7, 8].map(TechniCalc.ofFloat)))
  ).toMatchJsValue(Math.sqrt(122));
});
