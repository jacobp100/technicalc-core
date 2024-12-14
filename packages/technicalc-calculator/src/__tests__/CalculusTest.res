open Jest

test("differentiate x^2 at x=5", () => {
  open Value
  open AST
  let equation = Pow(X, OfInt(2))

  let value = differentiate(
    x => evalAt(~config=defaultConfig, ~context=emptyContext, ~args=None, ~x, equation),
    Value.ofInt(5),
  )

  expect(Value.toFloat(value))->toBe(10.)
})

test("integrate x^2 between [3, 6]", () => {
  open Value
  open AST
  let equation = Pow(X, OfInt(2))

  let value = integrate(
    x => evalAt(~config=defaultConfig, ~context=emptyContext, ~args=None, ~x, equation),
    Value.ofInt(3),
    Value.ofInt(6),
  )

  expect(Value.toFloat(value))->toBe(63.)
})
