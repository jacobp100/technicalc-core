open Jest

let openTag = `<math xmlns="http://www.w3.org/1998/Math/MathML" display="block">`
let closeTag = `</math>`

let create = elements =>
  Mml.create(elements)->StringUtil.slice(String.length(openTag), -String.length(closeTag))

test("formats numbers", () => {
  create([N1_S])->expect->toEqual(`<mrow><mn id="0:1">1</mn></mrow>`)

  create([N1_S, N2_S])->expect->toEqual(`<mrow><mn id="0:1">1</mn><mn id="1:2">2</mn></mrow>`)

  create([N1_S, N2_S, N3_S])
  ->expect
  ->toEqual(`<mrow><mn id="0:1">1</mn><mn id="1:2">2</mn><mn id="2:3">3</mn></mrow>`)

  create([N1_S, N2_S, N3_S, N4_S])
  ->expect
  ->toEqual(`<mrow><mn id="0:1">1</mn><mn>,</mn><mn id="1:2">2</mn><mn id="2:3">3</mn><mn id="3:4">4</mn></mrow>`)

  create([N1_S, N2_S, N3_S, N4_S, N5_S, N6_S, N7_S])
  ->expect
  ->toEqual(`<mrow><mn id="0:1">1</mn><mn>,</mn><mn id="1:2">2</mn><mn id="2:3">3</mn><mn id="3:4">4</mn><mn>,</mn><mn id="4:5">5</mn><mn id="5:6">6</mn><mn id="6:7">7</mn></mrow>`)

  create([N0_S, DecimalSeparator, N1_S, N2_S, N3_S, N4_S, N5_S, N6_S, N7_S])
  ->expect
  ->toEqual(`<mrow><mn id="0:1">0</mn><mn id="1:2">.</mn><mn id="2:3">1</mn><mn id="3:4">2</mn><mn id="4:5">3</mn><mn id="5:6">4</mn><mn id="6:7">5</mn><mn id="7:8">6</mn><mn id="8:9">7</mn></mrow>`)

  create([Hex, N2_S, N3_S, N4_S, N5_S, N6_S, N7_S])
  ->expect
  ->toEqual(`<mrow><mn id="0:1">0x</mn><mn id="1:2">2</mn><mn id="2:3">3</mn><mn id="3:4">4</mn><mn id="4:5">5</mn><mn id="5:6">6</mn><mn id="6:7">7</mn></mrow>`)
})

test("superscripts", () => {
  create([N1_S, Superscript1, N2_S, Arg])
  ->expect
  ->toEqual(`<mrow><msup id="0:4"><mn id=":1">1</mn><mrow><mn id="2:3">2</mn></mrow></msup></mrow>`)
})

test("invalid brackets", () => {
  create([N1_S, OpenBracket, N2_S, N3_S])
  ->expect
  ->toEqual(`<mrow><mn id="0:1">1</mn><mo id="1:2" class="invalid" stretchy="false">(</mo><mn id="2:3">2</mn><mn id="3:4">3</mn></mrow>`)

  create([N1_S, N2_S, CloseBracketS, N3_S])
  ->expect
  ->toEqual(`<mrow><mn id="0:1">1</mn><mn id="1:2">2</mn><mo id="2:3" class="invalid" stretchy="false">)</mo><mn id="3:4">3</mn></mrow>`)

  create([OpenBracket, N1_S, OpenBracket])
  ->expect
  ->toEqual(`<mrow><mo id="0:1" class="invalid" stretchy="false">(</mo><mn id="1:2">1</mn><mo id="2:3" class="invalid" stretchy="false">(</mo></mrow>`)
})

test("bracket states", () => {
  create([N1_S, OpenBracket, N2_S, CloseBracketS, N3_S])
  ->expect
  ->toEqual(`<mrow><mn id=\"0:1\">1</mn><mo id=\"1:2\">(</mo><mn id=\"2:3\">2</mn><mo id=\"3:4\">)</mo><mn id=\"4:5\">3</mn></mrow>`)

  create([N1_S, OpenBracket, N2_S, OpenBracket, N3_S, CloseBracketS, N4_S, CloseBracketS, N5_S])
  ->expect
  ->toEqual(`<mrow><mn id=\"0:1\">1</mn><mo id=\"1:2\">(</mo><mn id=\"2:3\">2</mn><mo id=\"3:4\">(</mo><mn id=\"4:5\">3</mn><mo id=\"5:6\">)</mo><mn id=\"6:7\">4</mn><mo id=\"7:8\">)</mo><mn id=\"8:9\">5</mn></mrow>`)
})

test("implicit multiplication indicator", () => {
  create([Frac2S, N2_S, Arg, N3_S, Arg])
  ->expect
  ->toEqual(`<mrow><mfrac id=\"0:5\"><mrow><mn id=\"1:2\">2</mn></mrow><mrow><mn id=\"3:4\">3</mn></mrow></mfrac></mrow>`)

  create([N1_S, Frac2S, N2_S, Arg, N3_S, Arg])
  ->expect
  ->toEqual(`<mrow><mn id=\"0:1\">1</mn><mo class=\"placeholder\" mathvariant=\"normal\">&#xb7;</mo><mfrac id=\"~1:6\"><mrow><mn id=\"2:3\">2</mn></mrow><mrow><mn id=\"4:5\">3</mn></mrow></mfrac></mrow>`)
})

test("abs-like functions", () => {
  create([Abs1S, N1_S, Arg])
  ->expect
  ->toEqual(`<mrow><mrow id=\"0:3\"><mo>|</mo><mrow><mn id=\"1:2\">1</mn></mrow><mo>|</mo></mrow></mrow>`)
})

test("functions with 2 arguments", () => {
  create([GCD2S, N1_S, Arg, N2_S, Arg])
  ->expect
  ->toEqual(`<mrow><mrow id=\"0:5\"><mi>gcd</mi><mo>(</mo><mrow><mn id=\"1:2\">1</mn></mrow><mo>,</mo><mrow><mn id=\"3:4\">2</mn></mrow><mo>)</mo></mrow></mrow>`)
})

test("series ranges", () => {
  create([Sum2, N1_S, Arg, N2_S, Arg])
  ->expect
  ->toEqual(`<mrow><mrow id=\"0:5\"><munderover><mo>&#x2211;</mo><mrow><mi>x</mi><mo>=</mo><mrow><mn id=\"1:2\">1</mn></mrow></mrow><mrow><mn id=\"3:4\">2</mn></mrow></munderover></mrow></mrow>`)

  create([Product2, N1_S, Arg, N2_S, Arg])
  ->expect
  ->toEqual(`<mrow><mrow id=\"0:5\"><munderover><mo>&#x220F;</mo><mrow><mi>x</mi><mo>=</mo><mrow><mn id=\"1:2\">1</mn></mrow></mrow><mrow><mn id=\"3:4\">2</mn></mrow></munderover></mrow></mrow>`)
})

test("npr", () => {
  create([NPR2, N1_S, Arg, N2_S, Arg])
  ->expect
  ->toEqual(`<mrow><mrow id=\"0:5\"><mmultiscripts><mi mathvariant=\"bold\">P</mi><mrow><mn id=\"3:4\">2</mn></mrow><none /><mprescripts /><mrow><mn id=\"1:2\">1</mn></mrow><none /></mmultiscripts></mrow></mrow>`)
})

test("tables", () => {
  create([TableNS({numRows: 2, numColumns: 1}), N1_S, Arg, N2_S, Arg])
  ->expect
  ->toEqual(`<mrow><mrow id=\"0:5\"><mo>[</mo><mtable><mtr><mtd><mrow><mn id=\"1:2\">1</mn></mrow></mtd></mtr><mtr><mtd><mrow><mn id=\"3:4\">2</mn></mrow></mtd></mtr></mtable><mo>]</mo></mrow></mrow>`)

  create([TableNS({numRows: 2, numColumns: 2}), N1_S, Arg, N2_S, Arg, N3_S, Arg, N4_S, Arg])
  ->expect
  ->toEqual(`<mrow><mrow id=\"0:9\"><mo>[</mo><mtable><mtr><mtd><mrow><mn id=\"1:2\">1</mn></mrow></mtd><mtd><mrow><mn id=\"3:4\">2</mn></mrow></mtd></mtr><mtr><mtd><mrow><mn id=\"5:6\">3</mn></mrow></mtd><mtd><mrow><mn id=\"7:8\">4</mn></mrow></mtd></mtr></mtable><mo>]</mo></mrow></mrow>`)
})

test("operators", () => {
  create([N1_S, Add, N2_S])
  ->expect
  ->toEqual(`<mrow><mn id="0:1">1</mn><mo id="1:2">+</mo><mn id="2:3">2</mn></mrow>`)
})

test("differentials", () => {
  create([Differential2, N1_S, Arg, N2_S, Arg])
  ->expect
  ->toEqual(`<mrow><mrow id=\"0:5\"><mfrac><mi mathvariant=\"normal\">d</mi><mi>dx</mi></mfrac><mrow><mn id=\"1:2\">1</mn></mrow><munder align=\"left\"><mo>|</mo><mrow><mi>x</mi><mo>=</mo><mrow><mn id=\"3:4\">2</mn></mrow></mrow></munder></mrow></mrow>`)
})

test("integrals", () => {
  create([Integral3, N1_S, Arg, N2_S, Arg, N3_S, Arg])
  ->expect
  ->toEqual(`<mrow><mrow id=\"0:7\"><msubsup><mo>&#x222B;</mo><mrow><mn id=\"1:2\">1</mn></mrow><mrow><mn id=\"3:4\">2</mn></mrow></msubsup><mrow><mn id=\"5:6\">3</mn></mrow><mi>dx</mi></mrow></mrow>`)
})

test("conjugate", () => {
  create([Conj])
  ->expect
  ->toEqual(`<mrow><msup id=\"0:1\"><mi class=\"placeholder\" mathvariant=\"normal\">&#x25a1;</mi><mo>&#x02217;</mo></msup></mrow>`)

  create([N1_S, Conj])
  ->expect
  ->toEqual(`<mrow><msup id=\":2\"><mrow><mn id=\"0:1\">1</mn></mrow><mo id=\"~1:\">&#x02217;</mo></msup></mrow>`)

  create([Frac2S, N1_S, Arg, N2_S, Arg, Conj])
  ->expect
  ->toEqual(`<mrow><msup id=\":6\"><mrow><mfrac id=\"0:5\"><mrow><mn id=\"1:2\">1</mn></mrow><mrow><mn id=\"3:4\">2</mn></mrow></mfrac></mrow><mo id=\"~5:\">&#x02217;</mo></msup></mrow>`)
})

test("transpose", () => {
  create([Transpose])
  ->expect
  ->toEqual(`<mrow><msup id=\"0:1\"><mi class=\"placeholder\" mathvariant=\"normal\">&#x25a1;</mi><mi>T</mi></msup></mrow>`)

  create([N1_S, Transpose])
  ->expect
  ->toEqual(`<mrow><msup id=\":2\"><mrow><mn id=\"0:1\">1</mn></mrow><mi id=\"~1:\">T</mi></msup></mrow>`)

  create([
    TableNS({numRows: 2, numColumns: 2}),
    N1_S,
    Arg,
    N2_S,
    Arg,
    N3_S,
    Arg,
    N4_S,
    Arg,
    Transpose,
  ])
  ->expect
  ->toEqual(`<mrow><msup id=\":10\"><mrow><mrow id=\"0:9\"><mo>[</mo><mtable><mtr><mtd><mrow><mn id=\"1:2\">1</mn></mrow></mtd><mtd><mrow><mn id=\"3:4\">2</mn></mrow></mtd></mtr><mtr><mtd><mrow><mn id=\"5:6\">3</mn></mrow></mtd><mtd><mrow><mn id=\"7:8\">4</mn></mrow></mtd></mtr></mtable><mo>]</mo></mrow></mrow><mi id=\"~9:\">T</mi></msup></mrow>`)
})
