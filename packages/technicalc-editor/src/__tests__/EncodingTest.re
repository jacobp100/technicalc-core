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
  let decoded = Encoding.decode(encoded)->Belt.Option.getExn;

  expect(value)->toEqual(decoded);
});

// Remove this at a later date
test("decodes legacy label element", (.) => {
  let labelIdentifier = TechniCalcCalculator.Encoding.encodeUint(258);
  let dummyEncoding =
    Encoding.encode([|CaptureGroupStart({placeholderMml: "test"})|]);
  let encoded =
    Js.String.slice(~from=0, ~to_=1, dummyEncoding)
    ++ labelIdentifier
    ++ Js.String.sliceToEnd(
         ~from=1 + Js.String.length(labelIdentifier),
         dummyEncoding,
       );
  let decoded = Encoding.decode(encoded)->Belt.Option.getExn;

  expect(decoded)
  ->toEqual([|
      CaptureGroupStart({placeholderMml: "test"}),
      CaptureGroupEndS,
    |]);
});
