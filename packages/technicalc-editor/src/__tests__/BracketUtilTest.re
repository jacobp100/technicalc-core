open Jest;
open BracketUtil;

test("finds range in brackts", (.) => {
  let ranges =
    bracketRanges([|OpenBracket, N1_S, N2_S, N3_S, CloseBracketS|]);

  let range1 = {start: 0, end_: 5, level: 0};
  expect(ranges)->toEqual([range1]);

  for (i in 0 to 5) {
    let range = bracketRange(ranges, i);
    expect(range)->toEqual(Some(range1));
  };
});

test("finds range in nested brackts", (.) => {
  let ranges =
    bracketRanges([|
      OpenBracket,
      N1_S,
      OpenBracket,
      N2_S,
      N3_S,
      N4_S,
      CloseBracketS,
      N5_S,
      CloseBracketS,
    |]);

  let range1 = {start: 0, end_: 9, level: 0};
  let range2 = {start: 2, end_: 7, level: 1};
  expect(ranges)->toEqual([range1, range2]);

  for (i in 0 to 1) {
    let range = bracketRange(ranges, i);
    expect(range)->toEqual(Some(range1));
  };

  for (i in 2 to 7) {
    let range = bracketRange(ranges, i);
    expect(range)->toEqual(Some(range2));
  };

  for (i in 8 to 9) {
    let range = bracketRange(ranges, i);
    expect(range)->toEqual(Some(range1));
  };
});

test("works with brackets in function arguments", (.) => {
  let ranges =
    bracketRanges([|
      OpenBracket,
      Abs1S,
      OpenBracket,
      CloseBracketS,
      Arg,
      CloseBracketS,
    |]);

  let range1 = {start: 0, end_: 6, level: 0};
  let range2 = {start: 2, end_: 4, level: 0};
  expect(ranges)->toEqual([range1, range2]);

  for (i in 0 to 1) {
    let range = bracketRange(ranges, i);
    expect(range)->toEqual(Some(range1));
  };

  for (i in 2 to 4) {
    let range = bracketRange(ranges, i);
    expect(range)->toEqual(Some(range2));
  };

  for (i in 5 to 6) {
    let range = bracketRange(ranges, i);
    expect(range)->toEqual(Some(range1));
  };
});

test("works with unmatched brackets in function arguments", (.) => {
  let ranges =
    bracketRanges([|OpenBracket, Abs1S, OpenBracket, Arg, CloseBracketS|]);

  let range1 = {start: 0, end_: 5, level: 0};
  expect(ranges)->toEqual([range1]);

  for (i in 0 to 5) {
    let range = bracketRange(ranges, i);
    expect(range)->toEqual(Some(range1));
  };
});
