open Jest
open AST_Types

let parse = v =>
  switch Value.parse(v) {
  | Ok(v) =>
    Ok({
      open TechniCalcCalculator.AST
      eval(~config=defaultConfig, ~context=emptyContext, v)
    })
  | Error(i) => Error(i)
  }

let asString = x =>
  switch x {
  | Ok(v) => Ok(TechniCalcCalculator.Formatting.toString(v))
  | Error(i) => Error(i)
  }

let ofMatrix = TechniCalcCalculator.Value.ofMatrix
let ofInt = TechniCalcCalculator.Value.ofInt

test("numbers", () => {
  parse([N1_S, N2_S, N3_S])->asString->expect->toEqual(Ok("123"))

  parse([N1_S, DecimalSeparator, N2_S, N3_S])->asString->expect->toEqual(Ok("1.23"))

  parse([N0_S, DecimalSeparator, N5_S])->asString->expect->toEqual(Ok("0.5"))

  parse([DecimalSeparator, N5_S])->asString->expect->toEqual(Ok("0.5"))

  parse([N5_S, DecimalSeparator])->asString->expect->toEqual(Ok("5"))

  parse([Sub, N3_S, DecimalSeparator, N5_S])->asString->expect->toEqual(Ok("-3.5"))
})

test("does not parse invalid numbers", () => {
  parse([N1_S, DecimalSeparator, N2_S, DecimalSeparator, N3_S])->expect->toEqual(Error(4))
})

test("superscripts on numbers", () => {
  parse([N2_S, Superscript1, N2_S, Arg])->asString->expect->toEqual(Ok("4"))

  parse([N1_S, N0_S, Superscript1, N2_S, Arg])->asString->expect->toEqual(Ok("100"))

  parse([N1_S, Superscript1, N2_S, Arg, N0_S, Superscript1, N2_S, Arg])->expect->toEqual(Error(8))
})

test("magnitudes", () => {
  parse([N1_S, Magnitude1, N3_S, Arg])->asString->expect->toEqual(Ok("1,000"))

  parse([N2_S, Superscript1, N2_S, Arg, Magnitude1, N3_S, Arg])->expect->toEqual(Ok(ofInt(4000)))
})

test("imaginary units", () => {
  let mulI = {
    open TechniCalcCalculator.Value
    mul(i)
  }

  parse([N2_S, ImaginaryUnitS])->expect->toEqual(Ok(ofInt(2)->mulI))

  parse([N1_S, N0_S, ImaginaryUnitS])->expect->toEqual(Ok(ofInt(10)->mulI))

  parse([N1_S, N0_S, Superscript1, N2_S, Arg, ImaginaryUnitS])
  ->expect
  ->toEqual(Ok(ofInt(100)->mulI))

  parse([N1_S, N0_S, ImaginaryUnitS, Superscript1, N2_S, Arg])->asString->expect->toEqual(Ok("-10"))

  parse([N1_S, N0_S, Superscript1, N2_S, Arg, ImaginaryUnitS, Superscript1, N2_S, Arg])
  ->expect
  ->toEqual(Ok(ofInt(-100)))
})

test("angles", () => {
  let pi = TechniCalcCalculator.Value.pi
  let mul = TechniCalcCalculator.Value.mul
  let div = TechniCalcCalculator.Value.div

  parse([N1_S, DegreeUnit])->expect->toEqual(Ok(div(pi, ofInt(180))))

  parse([N1_S, ArcMinuteUnit])->expect->toEqual(Ok(div(pi, ofInt(180 * 60))))

  parse([N1_S, ArcSecondUnit])->expect->toEqual(Ok(div(pi, ofInt(180 * 60 * 60))))

  parse([N1_S, DegreeUnit])->expect->toEqual(Ok(div(pi, ofInt(180))))

  parse([N1_S, DegreeUnit, N1_S, ArcMinuteUnit])
  ->expect
  ->toEqual(Ok(div(mul(ofInt(61), pi), ofInt(180 * 60))))

  parse([N1_S, ArcMinuteUnit, N1_S, DegreeUnit])->expect->toEqual(Error(4))

  parse([N1_S, DegreeUnit, N1_S, DegreeUnit])->expect->toEqual(Error(4))

  parse([N1_S, DegreeUnit, N1_S])->expect->toEqual(Error(2))

  parse([ConstPiS, RadianUnit])->expect->toEqual(Ok(pi))
})

test("mixed fractions", () => {
  parse([N2_S, Frac2S, N1_S, Arg, N5_S, Arg])->asString->expect->toEqual(Ok("2.2"))

  parse([N2_S, Frac2S, N1_S, Arg, N5_S, Arg, N4_S])->expect->toEqual(Error(6))

  parse([N2_S, Frac2S, N1_S, Arg, N5_S, Arg, Add, N4_S])->asString->expect->toEqual(Ok("6.2"))

  // Assume superscripts is the whole thing
  parse([N2_S, Frac2S, N1_S, Arg, N2_S, Arg, Superscript1, N2_S, Arg])
  ->asString
  ->expect
  ->toEqual(Ok("6.25"))
})

// test("mixed fraction angles", () => {
//   let pi = TechniCalcCalculator.Value.pi
//   let mul = TechniCalcCalculator.Value.mul
//   let div = TechniCalcCalculator.Value.div

//   parse([N1_S, Frac2S, N1_S, Arg, N2_S, Arg, DegreeUnit])
//   ->expect
//   ->toEqual(Ok(mul(div(ofInt(3), ofInt(2)), div(pi, ofInt(180)))))
// })

test("fractions with constants", () => {
  let pi = TechniCalcCalculator.Value.pi
  let div = TechniCalcCalculator.Value.div

  parse([Frac2S, N1_S, Arg, N2_S, Arg, ConstPiS])->expect->toEqual(Ok(div(pi, ofInt(2))))
  parse([N1_S, Frac2S, N1_S, Arg, N2_S, Arg, ConstPiS])->expect->toEqual(Error(6))
})

test("with bodmas", () => {
  parse([N1_S, Sub, N2_S, Add, N3_S])->asString->expect->toEqual(Ok("2"))

  parse([N4_S, Div, N2_S, Mul, N3_S])->asString->expect->toEqual(Ok("6"))
})

test("unary operators", () => {
  parse([Sub, N2_S])->asString->expect->toEqual(Ok("-2"))

  parse([Sub, Sub, Sub, N2_S])->asString->expect->toEqual(Ok("-2"))

  parse([N1_S, Sub, Sub, Sub, N2_S])->asString->expect->toEqual(Ok("-1"))
})

test("percent and mod", () => {
  parse([N1_S, N0_S, Add, N1_S, N0_S, Percent])->asString->expect->toEqual(Ok("11"))

  parse([N8_S, Percent, N3_S])->asString->expect->toEqual(Ok("2"))
})

test("brackets", () => {
  parse([N2_S, Mul, OpenBracket, N3_S, Add, N4_S, CloseBracketS, Mul, N2_S])
  ->asString
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
  ->asString
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
  ->asString
  ->expect
  ->toEqual(Ok("8"))
})

test("brackets with powers", () => {
  parse([OpenBracket, N1_S, Add, N2_S, CloseBracketS, Superscript1, N2_S, Arg])
  ->asString
  ->expect
  ->toEqual(Ok("9"))
})

test("functions", () => {
  parse([CosS, N0_S])->asString->expect->toEqual(Ok("1"))

  parse([CosS, OpenBracket, N0_S, CloseBracketS])->asString->expect->toEqual(Ok("1"))
})

test("iteration operators", () => {
  parse([Sum2, N0_S, Arg, N3_S, Arg, IterationXS, Add, N1_S])->asString->expect->toEqual(Ok("7"))

  parse([Sum2, N0_S, Arg, N3_S, Arg, OpenBracket, IterationXS, Add, N1_S, CloseBracketS])
  ->asString
  ->expect
  ->toEqual(Ok("10"))
})

test("postfixes", () => {
  parse([N3_S, Factorial])->asString->expect->toEqual(Ok("6"))
})

test("selects invalid operators", () => {
  parse([N1_S, Mul])->expect->toEqual(Error(2))
})

test("selects extraneous bracket elements", () => {
  parse([N1_S, OpenBracket, N2_S, N3_S])->expect->toEqual(Error(2))
  parse([N1_S, N2_S, CloseBracketS, N3_S])->expect->toEqual(Error(3))
})

test("log 0", () => {
  parse([Log, N0_S])->asString->expect->toEqual(Ok("NaN"))
})

test("tables default to zero", () => {
  let matrix = {
    open TechniCalcCalculator
    Matrix.make(~numRows=2, ~numColumns=2, [#Real(Real.one), #Zero, #Zero, #Real(Real.one)])
  }
  parse([TableNS({numRows: 2, numColumns: 2}), N1_S, Arg, Arg, Arg, N1_S, Arg])
  ->expect
  ->toEqual(Ok(ofMatrix(matrix)))
})
