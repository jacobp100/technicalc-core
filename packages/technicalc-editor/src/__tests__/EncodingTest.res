open Jest

test("encodes and decodes simple expression", () => {
  open AST_Types
  let value = [Superscript1, Arg]
  let encoded = Encoding.encode(value)
  let decoded = Encoding.decode(encoded)->Belt.Option.getExn

  expect(encoded)->toEqual("cvr")
  expect(value)->toEqual(decoded)
})

test("encodes and decodes", () => {
  open AST_Types
  let value = [N1_S, Sub, N2_S, Add, N3_S]
  let encoded = Encoding.encode(value)
  let decoded = Encoding.decode(encoded)->Belt.Option.getExn

  expect(value)->toEqual(decoded)

  let value = [
    CustomAtomS({
      symbol: {
        bold: false,
        italic: false,
        base: "Test",
        superscript: "",
        subscript: "",
      },
      value: TechniCalcCalculator.Encoding.encode(TechniCalcCalculator.Value.one),
    }),
  ]
  let encoded = Encoding.encode(value)
  let decoded = Encoding.decode(encoded)->Belt.Option.getExn

  expect(value)->toEqual(decoded)
})
