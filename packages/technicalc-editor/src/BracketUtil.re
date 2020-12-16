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
      let (ranges, i) =
        argCountExn(element)->iterArgs(~n=_, ast, ranges, i);
      iter(~ranges, ~stack, i + 1);
    };
  iter(~ranges, ~stack=[], index);
}
and iterArgs = (~n, ast, ranges, i) =>
  if (n > 0) {
    let (ranges, i) = prependBracketRanges(ast, ranges, i + 1);
    iterArgs(~n=n - 1, ast, ranges, i);
  } else {
    (ranges, i);
  };

let bracketRanges = (ast: array(t)) => {
  let (ranges, _) = prependBracketRanges(ast, [], 0);
  ranges;
};

let bracketRange = (ranges: list(bracketRange), index: int) => {
  let rec iter = (~current, ranges) =>
    switch (ranges) {
    | [{start, end_} as bracketRange, ...rest] =>
      let current =
        if (start <= index && end_ >= index) {
          Some(bracketRange);
        } else {
          current;
        };
      iter(~current, rest);
    | [] => current
    };
  iter(~current=None, ranges);
};
