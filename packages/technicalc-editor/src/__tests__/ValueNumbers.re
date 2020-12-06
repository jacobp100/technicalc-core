open Jest;

let parseEval = v =>
  switch (Value.parse(v)) {
  | Ok(v) => Some(TechniCalcCalculator.AST.eval(v))
  | _ => None
  };

let ofInt = TechniCalcCalculator.Value.ofInt;
let ofString = x =>
  TechniCalcCalculator.Value.ofString(x)->Belt.Option.getExn;

test("parses numbers", (.) => {
  parseEval([|N1_S, N2_S, N3_S|])->expect->toEqual(Some(ofString("123")));

  parseEval([|N1_S, DecimalSeparator, N2_S, N3_S|])
  ->expect
  ->toEqual(Some(ofString("1.23")));

  parseEval([|N0_S, DecimalSeparator, N5_S|])
  ->expect
  ->toEqual(Some(ofString("0.5")));

  parseEval([|DecimalSeparator, N5_S|])
  ->expect
  ->toEqual(Some(ofString("0.5")));

  parseEval([|N5_S, DecimalSeparator|])
  ->expect
  ->toEqual(Some(ofString("5")));

  parseEval([|Sub, N3_S, DecimalSeparator, N5_S|])
  ->expect
  ->toEqual(Some(ofString("-3.5")));
});

test("does not parse invalid numbers", (.) => {
  parseEval([|N1_S, DecimalSeparator, N2_S, DecimalSeparator, N3_S|])
  ->expect
  ->toEqual(None)
});

test("parses superscripts on numbers", (.) => {
  parseEval([|N2_S, Superscript1, N2_S, Arg|])
  ->expect
  ->toEqual(Some(ofString("4")));

  parseEval([|N1_S, N0_S, Superscript1, N2_S, Arg|])
  ->expect
  ->toEqual(Some(ofString("100")));

  parseEval([|N1_S, Superscript1, N2_S, Arg, N0_S, Superscript1, N2_S, Arg|])
  ->expect
  ->toEqual(None);
});

test("parses magnitudes", (.) => {
  parseEval([|N1_S, Magnitude1, N3_S, Arg|])
  ->expect
  ->toEqual(Some(ofString("1000")));

  parseEval([|N2_S, Superscript1, N2_S, Arg, Magnitude1, N3_S, Arg|])
  ->expect
  ->toEqual(Some(ofString("4000")));
});

test("parses imaginary units", (.) => {
  let mulI = TechniCalcCalculator.Value.(mul(i));

  parseEval([|N2_S, ImaginaryUnitS|])
  ->expect
  ->toEqual(Some(ofString("2")->mulI));

  parseEval([|N1_S, N0_S, ImaginaryUnitS|])
  ->expect
  ->toEqual(Some(ofString("10")->mulI));

  parseEval([|N1_S, N0_S, Superscript1, N2_S, Arg, ImaginaryUnitS|])
  ->expect
  ->toEqual(Some(ofString("100")->mulI));

  parseEval([|N1_S, N0_S, ImaginaryUnitS, Superscript1, N2_S, Arg|])
  ->expect
  ->toEqual(Some(ofString("-10")));

  parseEval([|
    N1_S,
    N0_S,
    Superscript1,
    N2_S,
    Arg,
    ImaginaryUnitS,
    Superscript1,
    N2_S,
    Arg,
  |])
  ->expect
  ->toEqual(Some(ofString("-100")));
});

test("angles", (.) => {
  let pi = TechniCalcCalculator.Value.pi;
  let mul = TechniCalcCalculator.Value.mul;
  let div = TechniCalcCalculator.Value.div;

  parseEval([|N1_S, DegreeUnit|])
  ->expect
  ->toEqual(Some(div(pi, ofInt(180))));

  parseEval([|N1_S, ArcMinuteUnit|])
  ->expect
  ->toEqual(Some(div(pi, ofInt(180 * 60))));

  parseEval([|N1_S, ArcSecondUnit|])
  ->expect
  ->toEqual(Some(div(pi, ofInt(180 * 60 * 60))));

  parseEval([|N1_S, DegreeUnit|])
  ->expect
  ->toEqual(Some(div(pi, ofInt(180))));

  parseEval([|N1_S, DegreeUnit, N1_S, ArcMinuteUnit|])
  ->expect
  ->toEqual(Some(div(mul(ofInt(61), pi), ofInt(180 * 60))));

  parseEval([|N1_S, ArcMinuteUnit, N1_S, DegreeUnit|])
  ->expect
  ->toEqual(None);

  parseEval([|N1_S, DegreeUnit, N1_S, DegreeUnit|])->expect->toEqual(None);

  parseEval([|N1_S, DegreeUnit, N1_S|])->expect->toEqual(None);
});
