open AST_Types;

type bracketRange = {
  start: int,
  [@bs.as "end"]
  end_: int,
  level: int,
};

let%private rec prependBracketRanges = (ast: array(t), ranges, index) => {
  let rec iter = (~ranges, ~stack, i) =>
    switch (Belt.Array.get(ast, i), stack) {
    | (Some(Arg) | None, _) => (ranges, i)
    | (Some(OpenBracket), _) => iter(~ranges, ~stack=[i, ...stack], i + 1)
    | (Some(CloseBracketS), [start, ...stack]) =>
      let end_ = i + 1;
      let level = Belt.List.length(stack);
      let range = {start, end_, level};
      iter(~ranges=[range, ...ranges], ~stack, i + 1);
    | (Some(element), _) =>
      let argCount = argCountExn(element);
      let (ranges, i) = iterArgs(~argCount, ast, ranges, i);
      iter(~ranges, ~stack, i + 1);
    };
  iter(~ranges, ~stack=[], index);
}
and iterArgs = (~argCount, ast, ranges, i) =>
  if (argCount > 0) {
    let (ranges, i) = prependBracketRanges(ast, ranges, i + 1);
    iterArgs(~argCount=argCount - 1, ast, ranges, i);
  } else {
    (ranges, i);
  };

// Returns bracket ranges in order of preference
// The first range to encapsulate an index is the one to display to the user
// Returns none on empty list for better value equality in React's useMemo
let bracketRanges = (ast: array(t)) =>
  switch (prependBracketRanges(ast, [], 0)) {
  | ([], _) => None
  | (ranges, _) => Belt.List.toArray(ranges)->Js.Array.reverseInPlace->Some
  };

let bracketRange = (ranges, index) =>
  switch (ranges) {
  | Some(ranges) =>
    Belt.Array.getByU(ranges, (. {start, end_}) => {
      start <= index && end_ >= index
    })
  | None => None
  };
