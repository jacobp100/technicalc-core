const TechniCalc = require("../Value.bs");
const TechniCalcTest = require("../ValueTestUtil.bs");
const { toMatchJsMatrix } = require("../__test-util__");

expect.extend({ toMatchJsMatrix });

test("mul", () => {
  const value = TechniCalc.mul(
    TechniCalcTest.matrixOfFloats(2, 2, [1, 1, 0, 1]),
    TechniCalc.ofVector([3, 4].map(TechniCalc.ofFloat))
  );
  expect(TechniCalcTest.toComplexFloatsMatrix(value)).toEqual([
    [[7, 0]],
    [[4, 0]],
  ]);
});
