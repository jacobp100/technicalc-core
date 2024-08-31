open Jest

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
  base: "y",
  superscript: "",
  subscript: "",
}

test("hint in function (continued)", () => {
  let elements: array<AST.t> = [
    /* 1 */ N1_S,
    /* 2 */ EquationNS({
      symbol: x,
      elements: [N1_S],
      body: TechniCalcCalculator.AST.One,
      arguments: [Some(x), Some(y)],
    }),
    /* 3 */ Frac2S,
    /* 4 */ N1_S,
    /* 5 */ Arg,
    /* 6 */ N2_S,
    /* 7 */ Arg,
    /* 8 */ Arg,
    /* 9 */ Arg,
    /* 10 */ N3_S,
  ]

  expect(HintMetadata.hint(elements, 0))->toEqual(None)
  expect(HintMetadata.hint(elements, 1))->toEqual(None)
  expect(HintMetadata.hint(elements, 2))->toEqual(
    Some(CaptureGroup({placeholder: x, isEmpty: false})),
  )
  expect(HintMetadata.hint(elements, 3))->toEqual(
    Some(CaptureGroup({placeholder: x, isEmpty: false})),
  )
  expect(HintMetadata.hint(elements, 4))->toEqual(
    Some(CaptureGroup({placeholder: x, isEmpty: false})),
  )
  expect(HintMetadata.hint(elements, 5))->toEqual(
    Some(CaptureGroup({placeholder: x, isEmpty: false})),
  )
  expect(HintMetadata.hint(elements, 6))->toEqual(
    Some(CaptureGroup({placeholder: x, isEmpty: false})),
  )
  expect(HintMetadata.hint(elements, 7))->toEqual(
    Some(CaptureGroup({placeholder: x, isEmpty: false})),
  )
  expect(HintMetadata.hint(elements, 8))->toEqual(
    Some(CaptureGroup({placeholder: y, isEmpty: true})),
  )
  expect(HintMetadata.hint(elements, 9))->toEqual(None)
  expect(HintMetadata.hint(elements, 10))->toEqual(None)
})
