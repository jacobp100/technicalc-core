open Jest;
open BracketUtil;

test("returns none when there's no ranges", (.) => {
  expect(bracketRanges([|OpenBracket|]))->toEqual(None);
  expect(bracketRanges([|CloseBracketS|]))->toEqual(None);
});

test("finds range in brackts", (.) => {
  let ranges =
    bracketRanges([|OpenBracket, N1_S, N2_S, N3_S, CloseBracketS|]);

  let topLevelRange = {start: 0, end_: 5, level: 0};
  expect(Belt.Option.getExn(ranges))->toEqual([|topLevelRange|]);

  expect(bracketRange(ranges, 0))->toEqual(Some(topLevelRange));
  expect(bracketRange(ranges, 1))->toEqual(Some(topLevelRange));
  expect(bracketRange(ranges, 2))->toEqual(Some(topLevelRange));
  expect(bracketRange(ranges, 3))->toEqual(Some(topLevelRange));
  expect(bracketRange(ranges, 4))->toEqual(Some(topLevelRange));
  expect(bracketRange(ranges, 5))->toEqual(Some(topLevelRange));
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

  let topLevelRange = {start: 0, end_: 9, level: 0};
  let nestedRange = {start: 2, end_: 7, level: 1};
  expect(Belt.Option.getExn(ranges))
  ->toEqual([|nestedRange, topLevelRange|]);

  expect(bracketRange(ranges, 0))->toEqual(Some(topLevelRange));
  expect(bracketRange(ranges, 1))->toEqual(Some(topLevelRange));
  expect(bracketRange(ranges, 2))->toEqual(Some(nestedRange));
  expect(bracketRange(ranges, 3))->toEqual(Some(nestedRange));
  expect(bracketRange(ranges, 4))->toEqual(Some(nestedRange));
  expect(bracketRange(ranges, 5))->toEqual(Some(nestedRange));
  expect(bracketRange(ranges, 6))->toEqual(Some(nestedRange));
  expect(bracketRange(ranges, 7))->toEqual(Some(nestedRange));
  expect(bracketRange(ranges, 8))->toEqual(Some(topLevelRange));
  expect(bracketRange(ranges, 9))->toEqual(Some(topLevelRange));
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

  let topLevelRange = {start: 0, end_: 6, level: 0};
  let innerRange = {start: 2, end_: 4, level: 0};
  expect(Belt.Option.getExn(ranges))
  ->toEqual([|innerRange, topLevelRange|]);

  expect(bracketRange(ranges, 0))->toEqual(Some(topLevelRange));
  expect(bracketRange(ranges, 1))->toEqual(Some(topLevelRange));
  expect(bracketRange(ranges, 2))->toEqual(Some(innerRange));
  expect(bracketRange(ranges, 3))->toEqual(Some(innerRange));
  expect(bracketRange(ranges, 4))->toEqual(Some(innerRange));
  expect(bracketRange(ranges, 5))->toEqual(Some(topLevelRange));
  expect(bracketRange(ranges, 6))->toEqual(Some(topLevelRange));
});

test("works with unmatched brackets in function arguments", (.) => {
  let ranges =
    bracketRanges([|OpenBracket, Abs1S, OpenBracket, Arg, CloseBracketS|]);

  let topLevelRange = {start: 0, end_: 5, level: 0};
  expect(Belt.Option.getExn(ranges))->toEqual([|topLevelRange|]);

  expect(bracketRange(ranges, 0))->toEqual(Some(topLevelRange));
  expect(bracketRange(ranges, 1))->toEqual(Some(topLevelRange));
  expect(bracketRange(ranges, 2))->toEqual(Some(topLevelRange));
  expect(bracketRange(ranges, 3))->toEqual(Some(topLevelRange));
  expect(bracketRange(ranges, 4))->toEqual(Some(topLevelRange));
  expect(bracketRange(ranges, 5))->toEqual(Some(topLevelRange));
});

test("works with multiple top-level bracket pairs", (.) => {
  let ranges =
    bracketRanges([|
      OpenBracket,
      CloseBracketS,
      OpenBracket,
      CloseBracketS,
      OpenBracket,
      CloseBracketS,
    |]);

  let topLevelRange1 = {start: 0, end_: 2, level: 0};
  let topLevelRange2 = {start: 2, end_: 4, level: 0};
  let topLevelRange3 = {start: 4, end_: 6, level: 0};
  expect(Belt.Option.getExn(ranges))
  ->toEqual([|topLevelRange1, topLevelRange2, topLevelRange3|]);

  expect(bracketRange(ranges, 0))->toEqual(Some(topLevelRange1));
  expect(bracketRange(ranges, 1))->toEqual(Some(topLevelRange1));
  expect(bracketRange(ranges, 2))->toEqual(Some(topLevelRange1));
  expect(bracketRange(ranges, 3))->toEqual(Some(topLevelRange2));
  expect(bracketRange(ranges, 4))->toEqual(Some(topLevelRange2));
  expect(bracketRange(ranges, 5))->toEqual(Some(topLevelRange3));
  expect(bracketRange(ranges, 6))->toEqual(Some(topLevelRange3));
});

test(
  "works with multiple top-level bracket pairs with function arguments", (.) => {
  let ranges =
    bracketRanges([|
      OpenBracket,
      CloseBracketS,
      OpenBracket,
      Abs1S,
      OpenBracket,
      CloseBracketS,
      Arg,
      CloseBracketS,
      OpenBracket,
      CloseBracketS,
    |]);

  let topLevelRange1 = {start: 0, end_: 2, level: 0};
  let topLevelRange2 = {start: 2, end_: 8, level: 0};
  let innerRange = {start: 4, end_: 6, level: 0};
  let topLevelRange3 = {start: 8, end_: 10, level: 0};
  expect(Belt.Option.getExn(ranges))
  ->toEqual([|topLevelRange1, innerRange, topLevelRange2, topLevelRange3|]);

  expect(bracketRange(ranges, 0))->toEqual(Some(topLevelRange1));
  expect(bracketRange(ranges, 1))->toEqual(Some(topLevelRange1));
  expect(bracketRange(ranges, 2))->toEqual(Some(topLevelRange1));
  expect(bracketRange(ranges, 3))->toEqual(Some(topLevelRange2));
  expect(bracketRange(ranges, 4))->toEqual(Some(innerRange));
  expect(bracketRange(ranges, 5))->toEqual(Some(innerRange));
  expect(bracketRange(ranges, 6))->toEqual(Some(innerRange));
  expect(bracketRange(ranges, 7))->toEqual(Some(topLevelRange2));
  expect(bracketRange(ranges, 8))->toEqual(Some(topLevelRange2));
  expect(bracketRange(ranges, 9))->toEqual(Some(topLevelRange3));
  expect(bracketRange(ranges, 10))->toEqual(Some(topLevelRange3));
});
