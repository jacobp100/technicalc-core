open Jest
open Real

test("simplifies division by two square roots", () => {
  expect(sqrt(ofInt(10)) / sqrt(ofInt(2)))->toEqual(sqrt(ofInt(5)))
  expect(sqrt(ofInt(1000)) / sqrt(ofInt(2)))->toEqual(ofInt(10) * sqrt(ofInt(5)))
})

test("tracks exp values through log", () => {
  expect(logExn(exp(ofInt(47))))->toEqual(ofInt(47))
})

test("simplifies square roots and exponentials", () => {
  expect(ofInt(2) * sqrt(ofInt(2)))->toEqual(ofInt(2) * sqrt(ofInt(2)))
  expect(sqrt(ofInt(1000)))->toEqual(ofInt(10) * sqrt(ofInt(10)))
  expect(sqrt(ofInt(4)))->toEqual(ofInt(2))
  expect(sqrt(ofInt(8)))->toEqual(ofInt(2) * sqrt(ofInt(2)))
  expect(sqrt(ofInt(6)))->toEqual(sqrt(ofInt(6)))
  expect(sqrt(ofInt(12)))->toEqual(ofInt(2) * sqrt(ofInt(3)))
  expect(sqrt(ofInt(0)))->toEqual(ofInt(0))
  expect(exp(ofInt(0)))->toEqual(ofInt(1))
  expect(exp(ofInt(1)))->toEqual(exp(ofInt(1)))
  expect(exp(ofInt(2)))->toEqual(exp(ofInt(2)))
  expect(exp(ofInt(3)))->toEqual(exp(ofInt(3)))
  expect(exp(ofInt(-1)))->toEqual(exp(ofInt(-1)))
})
