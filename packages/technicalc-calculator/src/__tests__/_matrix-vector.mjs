import * as TechniCalc from "../Value";
import * as TechniCalcTest from "../ValueTestUtil";
import { toMatchJsMatrix } from "../__test-util__/_index";

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
