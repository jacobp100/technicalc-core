open Jest;
open AST_Types;

let parseEval = v =>
  switch (Value.parse(v)) {
  | Ok(v) => Some(TechniCalcCalculator.AST.eval(v))
  | _ => None
  };

let ofString = x =>
  TechniCalcCalculator.Value.ofString(x)->Belt.Option.getExn;

test("Parses with bodmas", (.) => {
  parseEval([|N1_S, Sub, N2_S, Add, N3_S|])
  ->expect
  ->toEqual(Some(ofString("2")));

  parseEval([|N4_S, Div, N2_S, Mul, N3_S|])
  ->expect
  ->toEqual(Some(ofString("6")));
});

test("Parses unary operators", (.) => {
  parseEval([|Sub, N2_S|])->expect->toEqual(Some(ofString("-2")));

  parseEval([|Sub, Sub, Sub, N2_S|])
  ->expect
  ->toEqual(Some(ofString("-2")));

  parseEval([|N1_S, Sub, Sub, Sub, N2_S|])
  ->expect
  ->toEqual(Some(ofString("-1")));
});

test("Parses brackets", (.) => {
  parseEval([|
    N2_S,
    Mul,
    OpenBracket,
    N3_S,
    Add,
    N4_S,
    CloseBracketS,
    Mul,
    N2_S,
  |])
  ->expect
  ->toEqual(Some(ofString("28")))
});

test("Parses functions", (.) => {
  parseEval([|CosS, N0_S|])->expect->toEqual(Some(ofString("1")));

  parseEval([|CosS, OpenBracket, N0_S, CloseBracketS|])
  ->expect
  ->toEqual(Some(ofString("1")));
});

test("Parses iteration operators", (.) => {
  parseEval([|Sum2, N0_S, Arg, N3_S, Arg, VariableS("x"), Add, N1_S|])
  ->expect
  ->toEqual(Some(ofString("7")));

  parseEval([|
    Sum2,
    N0_S,
    Arg,
    N3_S,
    Arg,
    OpenBracket,
    VariableS("x"),
    Add,
    N1_S,
    CloseBracketS,
  |])
  ->expect
  ->toEqual(Some(ofString("10")));
});
