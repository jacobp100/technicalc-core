open Jest

let create = Tex.create

test("formats numbers", () => {
  create([N1_S])->expect->toEqual(`1`)

  create([N1_S, N2_S])->expect->toEqual(`12`)

  create([N1_S, N2_S, N3_S])->expect->toEqual(`123`)

  create([N1_S, N2_S, N3_S, N4_S])->expect->toEqual(`1,234`)

  create([N1_S, N2_S, N3_S, N4_S, N5_S, N6_S, N7_S])->expect->toEqual(`1,234,567`)

  create([N0_S, DecimalSeparator, N1_S, N2_S, N3_S, N4_S, N5_S, N6_S, N7_S])
  ->expect
  ->toEqual(`0.1234567`)

  create([Hex, N2_S, N3_S, N4_S, N5_S, N6_S, N7_S])->expect->toEqual(`\\rm{0x}234567`)
})

test("superscripts", () => {
  create([N1_S, Superscript1, N2_S, Arg])->expect->toEqual(`1^{2}`)
})

test("invalid brackets", () => {
  create([N1_S, OpenBracket, N2_S, N3_S])->expect->toEqual(`1(23`)

  create([N1_S, N2_S, CloseBracketS, N3_S])->expect->toEqual(`12)3`)

  create([OpenBracket, N1_S, OpenBracket])->expect->toEqual(`(1(`)
})

test("bracket states", () => {
  create([N1_S, OpenBracket, N2_S, CloseBracketS, N3_S])->expect->toEqual(`1\\left(2\\right)3`)

  create([N1_S, OpenBracket, N2_S, OpenBracket, N3_S, CloseBracketS, N4_S, CloseBracketS, N5_S])
  ->expect
  ->toEqual(`1\\left(2\\left(3\\right)4\\right)5`)
})

test("implicit multiplication indicator", () => {
  create([Frac2S, N2_S, Arg, N3_S, Arg])->expect->toEqual(`\\frac{2}{3}`)

  create([N1_S, Frac2S, N2_S, Arg, N3_S, Arg])->expect->toEqual(`1\\frac{2}{3}`)
})

test("abs-like functions", () => {
  create([Abs1S, N1_S, Arg])->expect->toEqual(`\\left|{1}\\right|`)
})

test("functions with 2 arguments", () => {
  create([GCD2S, N1_S, Arg, N2_S, Arg])->expect->toEqual(`\\gcd\\left({1}, {2}\\right)`)
})

test("series ranges", () => {
  create([Sum2, N1_S, Arg, N2_S, Arg])->expect->toEqual(`\\sum_{x={1}}^{2}`)

  create([Product2, N1_S, Arg, N2_S, Arg])->expect->toEqual(`\\prod_{x={1}}^{2}`)
})

test("npr", () => {
  create([NPR2, N1_S, Arg, N2_S, Arg])->expect->toEqual(`{}_{1}P{}_{2}`)
})

test("tables", () => {
  create([TableNS({numRows: 2, numColumns: 1}), N1_S, Arg, N2_S, Arg])
  ->expect
  ->toEqual(`\\left[\\begin{matrix}{1} \\\\ {2}\\end{matrix}\\right]`)

  create([TableNS({numRows: 2, numColumns: 2}), N1_S, Arg, N2_S, Arg, N3_S, Arg, N4_S, Arg])
  ->expect
  ->toEqual(`\\left[\\begin{matrix}{1} & {2} \\\\ {3} & {4}\\end{matrix}\\right]`)
})

test("operators", () => {
  create([N1_S, Add, N2_S])->expect->toEqual(`1+2`)
})

test("differentials", () => {
  create([Differential2, N1_S, Arg, N2_S, Arg])
  ->expect
  ->toEqual(`\\left.\\frac{d}{dx}{1}\\right|_{x={2}}`)
})

test("integrals", () => {
  create([Integral3, N1_S, Arg, N2_S, Arg, N3_S, Arg])->expect->toEqual(`\\int_{1}^{2} {3} dx`)
})
