import * as TechniCalc from "../Value/Value";
import * as TechniCalcTest from "../__test-util__/ValueTestUtil";
import { toMatchJsMatrix } from "../__test-util__/index";

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
