open Jest

test("inline equations with no placeholders", () => {
  let (body, arguments) =
    EquationMetadata.equationMetadata([
      CaptureGroupStart({placeholder: None}),
      CaptureGroupEndS,
      Add,
      CaptureGroupStart({placeholder: None}),
      CaptureGroupEndS,
    ])->Belt.Option.getExn

  expect(body)->toEqual([EquationArgumentS(0), Add, EquationArgumentS(1)])
  expect(arguments)->toEqual([None, None])
})

test("inline equations with placeholders", () => {
  let x: option<Symbol.t> = Some({
    bold: false,
    italic: false,
    base: "x",
    superscript: "",
    subscript: "",
  })
  let y: option<Symbol.t> = Some({
    bold: false,
    italic: false,
    base: "y",
    superscript: "",
    subscript: "",
  })
  let (body, arguments) =
    EquationMetadata.equationMetadata([
      Frac2S,
      CaptureGroupStart({placeholder: x}),
      CaptureGroupEndS,
      Add,
      CaptureGroupStart({placeholder: y}),
      CaptureGroupEndS,
      Arg,
      CaptureGroupStart({placeholder: x}),
      CaptureGroupEndS,
      Sub,
      CaptureGroupStart({placeholder: y}),
      CaptureGroupEndS,
      Arg,
    ])->Belt.Option.getExn

  expect(body)->toEqual([
    Frac2S,
    EquationArgumentS(0),
    Add,
    EquationArgumentS(1),
    Arg,
    EquationArgumentS(0),
    Sub,
    EquationArgumentS(1),
    Arg,
  ])
  expect(arguments)->toEqual([x, y])
})

test("parse equation", () => {
  let x: option<Symbol.t> = Some({
    bold: false,
    italic: false,
    base: "x",
    superscript: "",
    subscript: "",
  })
  let y: option<Symbol.t> = Some({
    bold: false,
    italic: false,
    base: "y",
    superscript: "",
    subscript: "",
  })
  let (body, arguments) =
    EquationMetadata.equationMetadata([
      Frac2S,
      CaptureGroupStart({placeholder: x}),
      CaptureGroupEndS,
      Add,
      CaptureGroupStart({placeholder: y}),
      CaptureGroupEndS,
      Arg,
      CaptureGroupStart({placeholder: x}),
      CaptureGroupEndS,
      Sub,
      CaptureGroupStart({placeholder: y}),
      CaptureGroupEndS,
      Arg,
    ])->Belt.Option.getExn

  let res = Value.parse([
    EquationNS({
      symbol: {
        bold: false,
        italic: false,
        base: "f",
        superscript: "",
        subscript: "",
      },
      body,
      arguments,
    }),
    N1_S,
    Arg,
    N2_S,
    Arg,
  ])

  let isOk = switch res {
  | Ok(_) => true
  | Error(_) => false
  }

  expect(isOk)->toBe(true)
})
