open Jest
open Value

test("42P89", () => {
  let value = nPr(ofInt(42), ofInt(89))
  expect(value)->toBe(zero)
})

test("tan 89P42", () => {
  // Accuracy goes out the window here - just check it's not nan
  // This used to crash
  let valueIsNaN = nPr(ofInt(89), ofInt(42))->tan->isNaN
  expect(valueIsNaN)->toBe(false)
})
