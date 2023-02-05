open Jest

let create = elements => Tex.create(~format=Stringifier.defaultFormat, elements)

test("formats numbers", () => {
  create([N1_S])->expect->toEqual(`1`)

  create([N1_S, N2_S])->expect->toEqual(`12`)

  create([N1_S, N2_S, N3_S])->expect->toEqual(`123`)

  create([N1_S, N2_S, N3_S, N4_S])->expect->toEqual(`1,234`)

  create([N1_S, N2_S, N3_S, N4_S, N5_S, N6_S, N7_S])->expect->toEqual(`1,234,567`)

  create([N0_S, DecimalSeparator, N1_S, N2_S, N3_S, N4_S, N5_S, N6_S, N7_S])
  ->expect
  ->toEqual(`0.1234567`)

  create([Hex, N2_S, N3_S, N4_S, N5_S, N6_S, N7_S])->expect->toEqual(`\\rm{0x}23,4567`)
})

test("superscripts", () => {
  create([N1_S, Superscript1, N2_S, Arg])->expect->toEqual(`1^{2}`)
})

test("invalid brackets", () => {
  create([N1_S, OpenBracket, N2_S, N3_S])->expect->toEqual(`1 ( 23`)

  create([N1_S, N2_S, CloseBracketS, N3_S])->expect->toEqual(`12 ) 3`)

  create([OpenBracket, N1_S, OpenBracket])->expect->toEqual(`( 1 (`)
})

test("bracket states", () => {
  create([N1_S, OpenBracket, N2_S, CloseBracketS, N3_S])->expect->toEqual(`1 \\left( 2 \\right) 3`)

  create([N1_S, OpenBracket, N2_S, OpenBracket, N3_S, CloseBracketS, N4_S, CloseBracketS, N5_S])
  ->expect
  ->toEqual(`1 \\left( 2 \\left( 3 \\right) 4 \\right) 5`)
})

test("implicit multiplication indicator", () => {
  create([Frac2S, N2_S, Arg, N3_S, Arg])->expect->toEqual(`\\frac{2}{3}`)

  create([N1_S, Frac2S, N2_S, Arg, N3_S, Arg])->expect->toEqual(`1 \\frac{2}{3}`)
})

test("abs-like functions", () => {
  create([Abs1S, N1_S, Arg])->expect->toEqual(`\\left|{1}\\right|`)
})

test("functions with 2 arguments", () => {
  create([GCD2S, N1_S, Arg, N2_S, Arg])->expect->toEqual(`\\gcd \\left(1, 2\\right)`)
})

test("angles", () => {
  create([DegreeUnit])->expect->toEqual("{}^{\\circ}")
  create([N1_S, DegreeUnit])->expect->toEqual("1^{\\circ}")

  create([ArcMinuteUnit])->expect->toEqual("'")
  create([N1_S, ArcMinuteUnit])->expect->toEqual("1'")

  create([ArcSecondUnit])->expect->toEqual("''")
  create([N1_S, ArcSecondUnit])->expect->toEqual("1''")

  create([RadianUnit])->expect->toEqual("{}^{r}")
  create([N1_S, RadianUnit])->expect->toEqual("1^{r}")

  create([GradianUnit])->expect->toEqual("{}^{g}")
  create([N1_S, GradianUnit])->expect->toEqual("1^{g}")
})

test("functions", () => {
  create([N2_S, SinS, Superscript1, N2_S, Arg, N3_S, N4_S, DegreeUnit])
  ->expect
  ->toEqual("2 \\sin^{2} 34^{\\circ}")
})

test("series ranges", () => {
  create([Sum2, N1_S, Arg, N2_S, Arg])->expect->toEqual(`\\sum_{x=1}^{2}`)

  create([Product2, N1_S, Arg, N2_S, Arg])->expect->toEqual(`\\prod_{x=1}^{2}`)
})

test("npr", () => {
  create([NPR2, N1_S, Arg, N2_S, Arg])->expect->toEqual(`{}_{1}P{}_{2}`)
})

test("tables", () => {
  create([TableNS({numRows: 2, numColumns: 1}), N1_S, Arg, N2_S, Arg])
  ->expect
  ->toEqual(`\\left[ \\begin{matrix} 1 \\\\ 2 \\end{matrix} \\right]`)

  create([TableNS({numRows: 2, numColumns: 2}), N1_S, Arg, N2_S, Arg, N3_S, Arg, N4_S, Arg])
  ->expect
  ->toEqual(`\\left[ \\begin{matrix} 1 & 2 \\\\ 3 & 4 \\end{matrix} \\right]`)
})

test("operators", () => {
  create([N1_S, Add, N2_S])->expect->toEqual(`1 + 2`)
})

test("differentials", () => {
  create([Differential2, N1_S, Arg, N2_S, Arg])
  ->expect
  ->toEqual(`\\left.\\frac{d}{dx}{1}\\right|_{x=2}`)
})

test("integrals", () => {
  create([Integral3, N1_S, Arg, N2_S, Arg, N3_S, Arg])->expect->toEqual(`\\int_{1}^{2} {3} dx`)
})
