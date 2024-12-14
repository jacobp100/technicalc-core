open AST_Types

type bracketRange = {
  start: int,
  end: int,
  level: int,
}

%%private(
  let rec prependBracketRanges = (ast: array<t>, rangesRev, index) => {
    let rec iter = (~rangesRev, ~stack, i) =>
      switch (Belt.Array.get(ast, i), stack) {
      | (Some(Arg) | None, _) => (rangesRev, i)
      | (Some(OpenBracket), _) => iter(~rangesRev, ~stack=list{i, ...stack}, i + 1)
      | (Some(CloseBracketS), list{start, ...stack}) =>
        let end = i + 1
        let level = Belt.List.length(stack)
        let range = {start, end, level}
        iter(~rangesRev=list{range, ...rangesRev}, ~stack, i + 1)
      | (Some(element), _) =>
        let argCount = argCountExn(element)
        let (rangesRev, i) = iterArgs(~argCount, ast, rangesRev, i)
        iter(~rangesRev, ~stack, i + 1)
      }
    iter(~rangesRev, ~stack=list{}, index)
  }
  and iterArgs = (~argCount, ast, rangesRev, i) =>
    if argCount > 0 {
      let (rangesRev, i) = prependBracketRanges(ast, rangesRev, i + 1)
      iterArgs(~argCount=argCount - 1, ast, rangesRev, i)
    } else {
      (rangesRev, i)
    }
)

// Returns bracket ranges in order of preference
// The first range to encapsulate an index is the one to display to the user
// Returns none on empty list for better value equality in React's useMemo
let bracketRanges = (ast: array<t>) =>
  switch prependBracketRanges(ast, list{}, 0) {
  | (list{}, _) => None
  | (ranges, _) => Belt.List.toArray(ranges)->ArrayUtil.reverseInPlace->Some
  }

let bracketRange = (ranges, index) =>
  switch ranges {
  | Some(ranges) => Belt.Array.getBy(ranges, ({start, end}) => start <= index && end >= index)
  | None => None
  }
