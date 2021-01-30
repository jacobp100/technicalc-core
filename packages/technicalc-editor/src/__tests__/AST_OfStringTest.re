open Jest;
open AST;

test("simple numbers", (.) => {
  expect(ofString("123"))->toEqual([|N1_S, N2_S, N3_S|])
});

test("bases", (.) => {
  expect(ofString("0b0101"))->toEqual([|Bin, N0_S, N1_S, N0_S, N1_S|]);
  expect(ofString("0o0123"))->toEqual([|Oct, N0_S, N1_S, N2_S, N3_S|]);
  expect(ofString("0x0123"))->toEqual([|Hex, N0_S, N1_S, N2_S, N3_S|]);
});

test("exponents", (.) => {
  let elements = [|N2_S, Superscript1, N3_S, Arg|];
  expect(ofString("2^3"))->toEqual(elements);
  expect(ofString("2^(3)"))->toEqual(elements);
});

test("does not form exponent with hexadecimal numbers", (.) => {
  expect(ofString("0xabcdef"))
  ->toEqual([|Hex, NA_S, NB_S, NC_S, ND_S, NE_S, NF_S|])
});

test("magnitudes", (.) => {
  let elements = [|N1_S, Magnitude1, N1_S, N0_S, Arg|];
  expect(ofString("1e10"))->toEqual(elements);
  expect(ofString("1E10"))->toEqual(elements);
  expect(ofString("1*10^10"))->toEqual(elements);
  expect(ofString("1x10^10"))->toEqual(elements);
  expect(ofString("1*10^(10)"))->toEqual(elements);
  expect(ofString("1x10^(10)"))->toEqual(elements);
});

test("magnitudes with signs", (.) => {
  expect(ofString("1e+10"))
  ->toEqual([|N1_S, Magnitude1, Add, N1_S, N0_S, Arg|]);
  expect(ofString("1e-10"))
  ->toEqual([|N1_S, Magnitude1, Sub, N1_S, N0_S, Arg|]);
});

test("brackets after elements with arguments", (.) => {
  expect(ofString("2^(3 + 4) + 5"))
  ->toEqual([|N2_S, Superscript1, N3_S, Add, N4_S, Arg, Add, N5_S|]);
  expect(ofString("2^(3 + 4"))
  ->toEqual([|N2_S, Superscript1, N3_S, Add, N4_S, Arg|]);
  expect(ofString("2^3 + 4"))
  ->toEqual([|N2_S, Superscript1, N3_S, Arg, Add, N4_S|]);

  expect(ofString("1e+10+10"))
  ->toEqual([|N1_S, Magnitude1, Add, N1_S, N0_S, Arg, Add, N1_S, N0_S|]);
  expect(ofString("1e-10-10"))
  ->toEqual([|N1_S, Magnitude1, Sub, N1_S, N0_S, Arg, Sub, N1_S, N0_S|]);
});

test("brackets not directly preceeding arguments", (.) => {
  expect(ofString("(1 + 2)^3"))
  ->toEqual([|
      OpenBracket,
      N1_S,
      Add,
      N2_S,
      CloseBracketS,
      Superscript1,
      N3_S,
      Arg,
    |])
});

test("nested arguments", (.) => {
  expect(ofString("4^3^2"))
  ->toEqual([|N4_S, Superscript1, N3_S, Superscript1, N2_S, Arg, Arg|])
});
