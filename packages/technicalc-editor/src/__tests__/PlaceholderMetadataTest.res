open Jest
open PlaceholderMetadata

test("placeholder metadata", () => {
  expect(
    placeholders([
      /* 0 */ Sqrt1S,
      /* 1 */ Frac2S,
      /* 2 */ TableNS({numRows: 2, numColumns: 2}),
      /* 3 */ Arg,
      /* 4 */ Arg,
      /* 5 */ Arg,
      /* 6 */ Arg /* Table */,
      /* 7 */ Arg /* Frac numerator */,
      /* 8 */ N2_S,
      /* 9 */ Arg /* Frac */,
      /* 10 */ Arg /* Sqrt */,
    ]),
  )->toEqual([])
})

test("function metadata", () => {
  let x: Symbol.t = {
    bold: false,
    italic: false,
    base: "x",
    superscript: "",
    subscript: "",
  }
  let y: Symbol.t = {
    bold: false,
    italic: false,
    base: "x",
    superscript: "",
    subscript: "",
  }

  expect(
    placeholders([
      /* 0 */ EquationNS({
        symbol: x,
        elements: [],
        body: Obj.magic(None),
        arguments: [Some(x), Some(y)],
      }),
      /* 1 */ N1_S,
      /* 2 */ Arg,
      /* 3 */ Arg,
    ]),
  )->toEqual([
    Explicit({index: 1, symbol: Some(x), elements: [N1_S]}),
    Explicit({index: 3, symbol: Some(y), elements: []}),
  ])
})
