open Jest
open AST_Types

let parse = v =>
  switch Value.parse(v) {
  | Ok(v) =>
    TechniCalcCalculator.Formatting.toString({
      open TechniCalcCalculator.AST
      eval(~config=defaultConfig, ~context=emptyContext, v)
    })->Ok
  | Error(i) => Error(i)
  }

test("parses with bodmas", (. ()) => {
  parse([N1_S, Sub, N2_S, Add, N3_S])->expect->toEqual(Ok("2"))

  parse([N4_S, Div, N2_S, Mul, N3_S])->expect->toEqual(Ok("6"))
})

test("parses unary operators", (. ()) => {
  parse([Sub, N2_S])->expect->toEqual(Ok("-2"))

  parse([Sub, Sub, Sub, N2_S])->expect->toEqual(Ok("-2"))

  parse([N1_S, Sub, Sub, Sub, N2_S])->expect->toEqual(Ok("-1"))
})

test("parses brackets", (. ()) => {
  parse([N2_S, Mul, OpenBracket, N3_S, Add, N4_S, CloseBracketS, Mul, N2_S])
  ->expect
  ->toEqual(Ok("28"))

  parse([
    N2_S,
    Mul,
    OpenBracket,
    N3_S,
    Mul,
    OpenBracket,
    N4_S,
    Add,
    N4_S,
    CloseBracketS,
    Mul,
    N3_S,
    CloseBracketS,
    Mul,
    N2_S,
  ])
  ->expect
  ->toEqual(Ok("288"))

  parse([
    N2_S,
    Mul,
    OpenBracket,
    OpenBracket,
    N4_S,
    Add,
    N4_S,
    CloseBracketS,
    Sub,
    OpenBracket,
    N3_S,
    Add,
    N3_S,
    CloseBracketS,
    CloseBracketS,
    Mul,
    N2_S,
  ])
  ->expect
  ->toEqual(Ok("8"))
})

test("parses brackets with powers", (. ()) => {
  parse([OpenBracket, N1_S, Add, N2_S, CloseBracketS, Superscript1, N2_S, Arg])
  ->expect
  ->toEqual(Ok("9"))
})

test("parses functions", (. ()) => {
  parse([CosS, N0_S])->expect->toEqual(Ok("1"))

  parse([CosS, OpenBracket, N0_S, CloseBracketS])->expect->toEqual(Ok("1"))
})

test("parses iteration operators", (. ()) => {
  parse([Sum2, N0_S, Arg, N3_S, Arg, IterationXS, Add, N1_S])->expect->toEqual(Ok("7"))

  parse([Sum2, N0_S, Arg, N3_S, Arg, OpenBracket, IterationXS, Add, N1_S, CloseBracketS])
  ->expect
  ->toEqual(Ok("10"))
})

test("parses postfixes", (. ()) => {
  parse([N3_S, Factorial])->expect->toEqual(Ok("6"))
})

test("selects invalid operators", (. ()) => {
  parse([N1_S, Mul])->expect->toEqual(Error(2))
})

test("selects extraneous bracket elements", (. ()) => {
  parse([N1_S, OpenBracket, N2_S, N3_S])->expect->toEqual(Error(2))
  parse([N1_S, N2_S, CloseBracketS, N3_S])->expect->toEqual(Error(3))
})

test("parses log 0", (. ()) => {
  parse([Log, N0_S])->expect->toEqual(Ok("NaN"))
})
