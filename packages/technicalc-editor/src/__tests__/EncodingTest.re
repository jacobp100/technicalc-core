open Jest;

test("encodes and decodes simple expression", (.) => {
  open AST_Types;
  let value = [|Superscript1, Arg|];
  let encoded = Encoding.encode(value);
  let decoded = Encoding.decode(encoded)->Belt.Option.getExn;

  expect(encoded)->toEqual("cvr");
  expect(value)->toEqual(decoded);
});

test("encodes and decodes", (.) => {
  open AST_Types;
  let value = [|N1_S, Sub, N2_S, Add, N3_S|];
  let encoded = Encoding.encode(value);
  let decoded = Encoding.decode(encoded)->Belt.Option.getExn;

  expect(value)->toEqual(decoded);

  let value = [|
    CustomAtomS({
      mml: "<mo>one</mo>",
      value: TechniCalcCalculator.Value.(encode(one)),
    }),
  |];
  let encoded = Encoding.encode(value);
  Js.log(encoded);
  let decoded = Encoding.decode(encoded)->Belt.Option.getExn;

  expect(value)->toEqual(decoded);
});
