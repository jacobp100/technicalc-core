open AST_Types

type bracketRange = {
  start: int,
  end: int,
  level: int,
}

%%private(
  let rec prependBracketRanges = (ast: array<t>, ranges, index) => {
    let rec iter = (~ranges, ~stack, i) =>
      switch (Belt.Array.get(ast, i), stack) {
      | (Some(Arg) | None, _) => (ranges, i)
      | (Some(OpenBracket), _) => iter(~ranges, ~stack=list{i, ...stack}, i + 1)
      | (Some(CloseBracketS), list{start, ...stack}) =>
        let end = i + 1
        let level = Belt.List.length(stack)
        let range = {start: start, end: end, level: level}
        iter(~ranges=list{range, ...ranges}, ~stack, i + 1)
      | (Some(element), _) =>
        let argCount = argCountExn(element)
        let (ranges, i) = iterArgs(~argCount, ast, ranges, i)
        iter(~ranges, ~stack, i + 1)
      }
    iter(~ranges, ~stack=list{}, index)
  }
  and iterArgs = (~argCount, ast, ranges, i) =>
    if argCount > 0 {
      let (ranges, i) = prependBracketRanges(ast, ranges, i + 1)
      iterArgs(~argCount=argCount - 1, ast, ranges, i)
    } else {
      (ranges, i)
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
  | Some(ranges) => Belt.Array.getByU(ranges, (. {start, end}) => start <= index && end >= index)
  | None => None
  }
