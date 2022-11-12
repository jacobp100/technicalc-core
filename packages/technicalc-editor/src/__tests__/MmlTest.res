open Jest

let openTag = j`<math xmlns="http://www.w3.org/1998/Math/MathML" display="block">`
let closeTag = j`</math>`
let openRow = j`$openTag<mrow>`
let closeRow = j`</mrow>$closeTag`
let invalidAttrs = j`class="invalid" stretchy="false"`

test("formats numbers", (. ()) => {
  Mml.create([N1_S])->expect->toEqual(j`$openTag<mn id="0:1">1</mn>$closeTag`)

  Mml.create([N1_S, N2_S])
  ->expect
  ->toEqual(j`$openRow<mn id="0:1">1</mn><mn id="1:2">2</mn>$closeRow`)

  Mml.create([N1_S, N2_S, N3_S])
  ->expect
  ->toEqual(j`$openRow<mn id="0:1">1</mn><mn id="1:2">2</mn><mn id="2:3">3</mn>$closeRow`)

  Mml.create([N1_S, N2_S, N3_S, N4_S])
  ->expect
  ->toEqual(j`$openRow<mn id="0:1">1</mn><mn>,</mn><mn id="1:2">2</mn><mn id="2:3">3</mn><mn id="3:4">4</mn>$closeRow`)

  Mml.create([N1_S, N2_S, N3_S, N4_S, N5_S, N6_S, N7_S])
  ->expect
  ->toEqual(j`$openRow<mn id="0:1">1</mn><mn>,</mn><mn id="1:2">2</mn><mn id="2:3">3</mn><mn id="3:4">4</mn><mn>,</mn><mn id="4:5">5</mn><mn id="5:6">6</mn><mn id="6:7">7</mn>$closeRow`)

  Mml.create([N0_S, DecimalSeparator, N1_S, N2_S, N3_S, N4_S, N5_S, N6_S, N7_S])
  ->expect
  ->toEqual(j`$openRow<mn id="0:1">0</mn><mn id="1:2">.</mn><mn id="2:3">1</mn><mn id="3:4">2</mn><mn id="4:5">3</mn><mn id="5:6">4</mn><mn id="6:7">5</mn><mn id="7:8">6</mn><mn id="8:9">7</mn>$closeRow`)

  Mml.create([Hex, N2_S, N3_S, N4_S, N5_S, N6_S, N7_S])
  ->expect
  ->toEqual(j`$openRow<mn id="0:1">0x</mn><mn id="1:2">2</mn><mn id="2:3">3</mn><mn id="3:4">4</mn><mn id="4:5">5</mn><mn id="5:6">6</mn><mn id="6:7">7</mn>$closeRow`)
})

test("superscripts", (. ()) => {
  Mml.create([N1_S, Superscript1, N2_S, Arg])
  ->expect
  ->toEqual(j`$openTag<msup id="0:4"><mn id=":1">1</mn><mn id="2:3">2</mn></msup>$closeTag`)
})

test("invalid brackets", (. ()) => {
  Mml.create([N1_S, OpenBracket, N2_S, N3_S])
  ->expect
  ->toEqual(j`$openRow<mn id="0:1">1</mn><mo id="1:2" $invalidAttrs>(</mo><mn id="2:3">2</mn><mn id="3:4">3</mn>$closeRow`)
  Mml.create([N1_S, N2_S, CloseBracketS, N3_S])
  ->expect
  ->toEqual(j`$openRow<mn id="0:1">1</mn><mn id="1:2">2</mn><mo id="2:3" $invalidAttrs>)</mo><mn id="3:4">3</mn>$closeRow`)
})

test("bracket states", (. ()) => {
  Mml.create([N1_S, OpenBracket, N2_S, CloseBracketS, N3_S])
  ->expect
  ->toEqual(j`$openRow<mn id="0:1">1</mn><mrow><mo id="1:2">(</mo><mn id="2:3">2</mn><mo id="3:4">)</mo></mrow><mn id="4:5">3</mn>$closeRow`)

  Mml.create([N1_S, OpenBracket, N2_S, OpenBracket, N3_S, CloseBracketS, N4_S, CloseBracketS, N5_S])
  ->expect
  ->toEqual(j`$openRow<mn id="0:1">1</mn><mrow><mo id="1:2">(</mo><mn id="2:3">2</mn><mrow><mo id="3:4">(</mo><mn id="4:5">3</mn><mo id="5:6">)</mo></mrow><mn id="6:7">4</mn><mo id="7:8">)</mo></mrow><mn id="8:9">5</mn>$closeRow`)
})

test("implicit multiplication indicator", (. ()) => {
  Mml.create([Frac2S, N2_S, Arg, N3_S, Arg])
  ->expect
  ->toEqual(j`$openTag<mfrac id="0:5"><mn id="1:2">2</mn><mn id="3:4">3</mn></mfrac>$closeTag`)

  Mml.create([N1_S, Frac2S, N2_S, Arg, N3_S, Arg])
  ->expect
  ->toEqual(j`$openRow<mn id="0:1">1</mn><mo class="placeholder" mathvariant="normal">&#xb7;</mo><mfrac id="1:6" data-selection-before="avoid"><mn id="2:3">2</mn><mn id="4:5">3</mn></mfrac>$closeRow`)
})

test("abs-like functions", (. ()) => {
  Mml.create([Abs1S, N1_S, Arg])
  ->expect
  ->toEqual(j`$openTag<mrow id="0:3"><mo>|</mo><mn id="1:2">1</mn><mo>|</mo></mrow>$closeTag`)
})

test("functions with 2 arguments", (. ()) => {
  Mml.create([GCD2S, N1_S, Arg, N2_S, Arg])
  ->expect
  ->toEqual(j`$openTag<mrow id="0:5"><mi>gcd</mi><mo>(</mo><mn id="1:2">1</mn><mo>,</mo><mn id="3:4">2</mn><mo>)</mo></mrow>$closeTag`)
})

test("series ranges", (. ()) => {
  Mml.create([Sum2, N1_S, Arg, N2_S, Arg])
  ->expect
  ->toEqual(j`$openTag<mrow id="0:5"><munderover><mo>&#x2211;</mo><mrow><mi>x</mi><mo>=</mo><mn id="1:2">1</mn></mrow><mn id="3:4">2</mn></munderover></mrow>$closeTag`)

  Mml.create([Product2, N1_S, Arg, N2_S, Arg])
  ->expect
  ->toEqual(j`$openTag<mrow id="0:5"><munderover><mo>&#x220F;</mo><mrow><mi>x</mi><mo>=</mo><mn id="1:2">1</mn></mrow><mn id="3:4">2</mn></munderover></mrow>$closeTag`)
})

test("npr", (. ()) => {
  Mml.create([NPR2, N1_S, Arg, N2_S, Arg])
  ->expect
  ->toEqual(j`$openTag<mrow id="0:5"><mmultiscripts><mi mathvariant="bold">P</mi><mn id="3:4">2</mn><none /><mprescripts /><mn id="1:2">1</mn><none /></mmultiscripts></mrow>$closeTag`)
})

test("tables", (. ()) => {
  Mml.create([TableNS({numRows: 2, numColumns: 1}), N1_S, Arg, N2_S, Arg])
  ->expect
  ->toEqual(j`$openTag<mrow id="0:5"><mo>[</mo><mtable><mtr><mtd><mn id="1:2">1</mn></mtd></mtr><mtr><mtd><mn id="3:4">2</mn></mtd></mtr></mtable><mo>]</mo></mrow>$closeTag`)

  Mml.create([TableNS({numRows: 2, numColumns: 2}), N1_S, Arg, N2_S, Arg, N3_S, Arg, N4_S, Arg])
  ->expect
  ->toEqual(j`$openTag<mrow id="0:9"><mo>[</mo><mtable><mtr><mtd><mn id="1:2">1</mn></mtd><mtd><mn id="3:4">2</mn></mtd></mtr><mtr><mtd><mn id="5:6">3</mn></mtd><mtd><mn id="7:8">4</mn></mtd></mtr></mtable><mo>]</mo></mrow>$closeTag`)
})

test("operators", (. ()) => {
  Mml.create([N1_S, Add, N2_S])
  ->expect
  ->toEqual(j`$openRow<mn id="0:1">1</mn><mo id="1:2">+</mo><mn id="2:3">2</mn>$closeRow`)
})

test("differentials", (. ()) => {
  Mml.create([Differential2, N1_S, Arg, N2_S, Arg])
  ->expect
  ->toEqual(j`$openTag<mrow id="0:5"><mfrac><mi mathvariant="normal">d</mi><mi>dx</mi></mfrac><mn id="1:2">1</mn><munder align="left"><mo>|</mo><mrow><mi>x</mi><mo>=</mo><mn id="3:4">2</mn></mrow></munder></mrow>$closeTag`)
})

test("integrals", (. ()) => {
  Mml.create([Integral3, N1_S, Arg, N2_S, Arg, N3_S, Arg])
  ->expect
  ->toEqual(j`$openTag<mrow id="0:7"><msubsup><mo>&#x222B;</mo><mn id="1:2">1</mn><mn id="3:4">2</mn></msubsup><mn id="5:6">3</mn><mi>dx</mi></mrow>$closeTag`)
})
