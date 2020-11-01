const { toMatchJsValue } = require("../__test-util__");
const { trigValues } = require("../__test-util__/math-native");
const TechniCalc = require("../Value.bs");
const TechniCalcTest = require("../ValueTestUtil.bs");

expect.extend({ toMatchJsValue });

const tanInfiniteValues = new Set([
  "1pi/2",
  "3pi/2",
  "5pi/2",
  "7pi/2",
  "9pi/2",
  "11pi/2",
  "-1pi/2",
  "-3pi/2",
  "-5pi/2",
  "-7pi/2",
  "-9pi/2",
  "-11pi/2",
]);

test.each(trigValues)("tan(%s)", (v) => {
  const actual = TechniCalc.tan(v.techniCalcValue);
  if (tanInfiniteValues.has(v.title)) {
    expect(TechniCalcTest.toString(actual)).toBe("NaN");
  } else {
    const expected = Math.tan(v.jsValue);
    expect(actual).toMatchJsValue(expected);
  }
});
