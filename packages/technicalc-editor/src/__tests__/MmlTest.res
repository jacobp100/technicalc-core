open Jest

let openTag = j`<math xmlns="http://www.w3.org/1998/Math/MathML" display="block">`
let closeTag = j`</math>`
let openRow = j`$openTag<mrow>`
let closeRow = j`</mrow>$closeTag`
let invalidAttrs = j`class="invalid" stretchy="false"`

test("gormats numbers", (. ()) => {
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
