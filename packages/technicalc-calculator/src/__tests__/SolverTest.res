open Jest

let truncateTo = (~dp, x) => {
  let factor = 10. ** Belt.Int.toFloat(dp)
  truncate(x *. factor)->Belt.Int.toFloat /. factor
}

test("solves sin(x) starting at 1", (. ()) => {
  open AST
  let equation = Sin(X)
  let initialGuess = OfInt(1)

  let value = solveRoot(~config=defaultConfig, ~context=AST_Context.empty, equation, initialGuess)

  let dp = 8
  expect(Value.toFloat(value)->truncateTo(~dp, _))->toBe(0.)
})

test("solves sin(x) starting at 3", (. ()) => {
  open AST
  let equation = Sin(X)
  let initialGuess = OfInt(3)

  let value = solveRoot(~config=defaultConfig, ~context=AST_Context.empty, equation, initialGuess)

  let dp = 6
  expect(Value.toFloat(value)->truncateTo(~dp, _))->toBe(Js.Math._PI->truncateTo(~dp, _))
})

test("solves x^5 - 2", (. ()) => {
  open AST
  let equation = Sub(Pow(X, OfInt(5)), OfInt(2))
  let initialGuess = OfInt(1)

  let value = solveRoot(~config=defaultConfig, ~context=AST_Context.empty, equation, initialGuess)

  let dp = 8
  expect(Value.toFloat(value)->truncateTo(~dp, _))->toBe((2. ** (1. /. 5.))->truncateTo(~dp, _))
})

test("solves x^5 - 6", (. ()) => {
  open AST
  let equation = Sub(Pow(X, OfInt(5)), OfInt(6))
  let initialGuess = OfInt(1)

  let value = solveRoot(~config=defaultConfig, ~context=AST_Context.empty, equation, initialGuess)

  let dp = 8
  expect(Value.toFloat(value)->truncateTo(~dp, _))->toBe((6. ** (1. /. 5.))->truncateTo(~dp, _))
})

test("solves x^5 - 6 starting at 1000", (. ()) => {
  open AST
  let equation = Sub(Pow(X, OfInt(5)), OfInt(6))
  let initialGuess = OfInt(1000)

  let value = solveRoot(~config=defaultConfig, ~context=AST_Context.empty, equation, initialGuess)

  let dp = 8
  expect(Value.toFloat(value)->truncateTo(~dp, _))->toBe((6. ** (1. /. 5.))->truncateTo(~dp, _))
})

test("solves 6x^3 - 5x^2 - 17x + 6", (. ()) => {
  open AST
  let a = Mul(OfInt(6), Pow(X, OfInt(3)))
  let b = Mul(OfInt(-5), Pow(X, OfInt(2)))
  let c = Mul(OfInt(-17), X)
  let d = OfInt(6)
  let equation = Add(a, Add(b, Add(c, d)))
  let initialGuess = OfInt(1000)

  let value = solveRoot(~config=defaultConfig, ~context=AST_Context.empty, equation, initialGuess)

  let dp = 8
  expect(Value.toFloat(value)->truncateTo(~dp, _))->toBe(2.)
})

test("solves system of 2 equations", (. ()) => {
  open Value
  expect(var2(ofInt(2), ofInt(3), ofInt(4), ofInt(5), ofInt(6), ofInt(7)))->toEqual((
    ofInt(-1),
    ofInt(2),
  ))
})

test("solves system of 2 equations", (. ()) => {
  open Value
  expect(var2(ofInt(2), ofInt(3), ofInt(4), ofInt(5), ofInt(6), ofInt(7)))->toEqual((
    ofInt(-1),
    ofInt(2),
  ))
})

test("solves system of 3 equations", (. ()) => {
  open Value
  expect(
    var3(
      ofInt(3),
      ofInt(2),
      ofInt(-1),
      ofInt(6),
      ofInt(-2),
      ofInt(2),
      ofInt(1),
      ofInt(3),
      ofInt(1),
      ofInt(1),
      ofInt(1),
      ofInt(4),
    ),
  )->toEqual((ofInt(1), ofInt(2), ofInt(1)))
})

test("solver bug report", (. ()) => {
  open AST
  let x = X
  let ofFloat = a => OfFloat(a)
  let \"+" = (a, b) => Add(a, b)
  let \"-" = (a, b) => Sub(a, b)
  let \"*" = (a, b) => Mul(a, b)
  // let (/) = (a, b) => Div(a, b);
  let \"**" = (a, b) => Pow(a, b)

  let equation =
    ofFloat(18.75) -
    ofFloat(1. /. 4.) * x ** ofFloat(2.) +
    ofFloat(2.5) * x +
    (x + ofFloat(5.)) * ofFloat(1. /. 2.) * (ofFloat(-1.) * x + ofFloat(2.5))

  let value = solveRoot(~config=defaultConfig, ~context=AST_Context.empty, equation, ofFloat(0.))

  expect(Value.toFloat(value))->toBe(-5.)
})

test("solver bug report 2", (. ()) => {
  open AST
  let x = X
  let ofInt = a => OfInt(a)
  let \"+" = (a, b) => Add(a, b)
  let \"-" = (a, b) => Sub(a, b)
  let \"*" = (a, b) => Mul(a, b)
  let \"/" = (a, b) => Div(a, b)
  let \"**" = (a, b) => Pow(a, b)

  let equation = (ofInt(3) * x - (x + ofInt(3)) * ofInt(3)) / (ofInt(9) * x ** ofInt(2)) + ofInt(1)

  let value = solveRoot(~config=defaultConfig, ~context=AST_Context.empty, equation, ofInt(1))

  expect(Value.toFloat(value))->toBe(1.)
})
