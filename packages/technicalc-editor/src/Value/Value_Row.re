open Value_Builders;
open Value_Types;

type parseResult =
  | Ok(node)
  | Error(int)
  | UnknownError;

let rec parseRest = (~current=None, elements) =>
  switch (current, elements) {
  | (Some(a), [Resolved(next), ...rest]) =>
    parseRest(~current=Some(Node.Mul(a, next)), rest)
  | (None, [Resolved(next), ...rest]) =>
    parseRest(~current=Some(next), rest)
  | (Some(a), [Unresolved(Percent, _, _)]) => Ok(Percent(a))
  | (_, [UnresolvedFunction(_, _, i') | Unresolved(_, _, i'), ..._]) =>
    Error(i')
  | (Some(v), []) => Ok(v)
  | (None, []) => UnknownError
  };
let next = parseRest;

let rec parsePostfixes = elements =>
  switch (elements) {
  | [
      Resolved(next),
      Unresolved(UnitConversion({fromUnits, toUnits}), _, _),
      ...rest,
    ] =>
    parsePostfixes([
      Resolved(Convert({body: next, fromUnits, toUnits})),
      ...rest,
    ])
  | [Resolved(next), Unresolved(Factorial, _, _), ...rest] =>
    parsePostfixes([Resolved(Factorial(next)), ...rest])
  | [Resolved(next), Unresolved(Conj, _, _), ...rest] =>
    parsePostfixes([Resolved(Conj(next)), ...rest])
  | _ => next(elements)
  };
let next = parsePostfixes;

type angleState = option((Value_Types.node, AST_ReduceMap.angle));

let parseNumbers = elements => {
  let next' = (numberState, angleState: angleState, rest) => {
    switch (Value_NumberParser.toNode(numberState), angleState) {
    | (None, Some((angleAccum, _))) =>
      next([Resolved(angleAccum), ...rest])
    | (Some(number), None) => next([Resolved(number), ...rest])
    | _ => next(elements)
    };
  };
  let rec iter = (numberState, angleState: angleState, rest) => {
    switch (rest) {
    | [Unresolved(Angle(angle), i, _), ...rest] =>
      let number =
        switch (Value_NumberParser.toNode(numberState), angle) {
        | (Some(number), Radian) => Some(Node.OfRad(number))
        | (Some(number), Degree) => Some(Node.OfDeg(number))
        | (Some(number), ArcMinute) => Some(OfArcMin(number))
        | (Some(number), ArcSecond) => Some(OfArcSec(number))
        | (Some(number), Gradian) => Some(OfGrad(number))
        | (None, _) => None
        };
      switch (number, angleState, angle) {
      | (Some(number), None, _) =>
        iter(Value_NumberParser.empty, Some((number, angle)), rest)
      | (Some(number), Some((angleAccum, Degree)), ArcMinute | ArcSecond)
      | (Some(number), Some((angleAccum, ArcMinute)), ArcSecond) =>
        iter(
          Value_NumberParser.empty,
          Some((Add(angleAccum, number), angle)),
          rest,
        )
      | _ => Error(i)
      };
    | [Unresolved(element, _, _), ...after] =>
      switch (Value_NumberParser.reduce(numberState, element)) {
      | Some(s) => iter(s, angleState, after)
      | None => next'(numberState, angleState, rest)
      }
    | _ => next'(numberState, angleState, rest)
    };
  };
  iter(Value_NumberParser.empty, None, elements);
};
let next = parseNumbers;

let rec parseUnary = elements =>
  switch (elements) {
  | [Unresolved(Operator((Add | Sub) as op), _, i'), ...rest] =>
    switch (parseUnary(rest)) {
    | Ok(root) =>
      let root = op == Sub ? Node.Neg(root) : root;
      Ok(root);
    | Error(_) as e => e
    | UnknownError => Error(i')
    }
  | _ => next(elements)
  };
let next = parseUnary;

let rec parseParenFreeFunctions = elements => {
  let rec iter = (after, index) =>
    switch (after) {
    | [UnresolvedFunction(fn, _, i'), ...after] =>
      switch (parseParenFreeFunctions(after)) {
      | Ok(arg) =>
        ListUtil.takeUpto(elements, index)
        ->Belt.List.concat([Resolved(handleFunction(arg, fn))])
        ->next
      | Error(_) as e => e
      | UnknownError => Error(i')
      }
    | [_, ...after] => iter(after, index + 1)
    | [] => next(elements)
    };
  iter(elements, 0);
};
let next = parseParenFreeFunctions;

let binaryOperatorParser = (~operatorHandled, ~next) => {
  let rec iter = (unaryPosition, current, after, index) =>
    switch (after) {
    | [Unresolved(Operator(op), _, i'), ...after] =>
      let nextAccum =
        !unaryPosition && operatorHandled(. op)
          ? Some((after, index, op, i')) : current;
      iter(true, nextAccum, after, index + 1);
    | [_, ...after] => iter(false, current, after, index + 1)
    | [] => current
    };
  let rec inner = elements =>
    switch (iter(true, None, elements, 0)) {
    | Some((after, index, op, i')) =>
      switch (ListUtil.takeUpto(elements, index)->inner, next(after)) {
      | (Ok(before), Ok(after)) => Ok(handleOp(op, before, after))
      | (Error(_) as e, _)
      | (_, Error(_) as e) => e
      | (UnknownError, _)
      | (_, UnknownError) => Error(i')
      }
    | None => next(elements)
    };
  inner;
};

let parseMulDiv =
  binaryOperatorParser(
    ~operatorHandled=(. op) => op == Mul || op == Div || op == Dot,
    ~next,
  );
let next = parseMulDiv;

let parseAddSub =
  binaryOperatorParser(
    ~operatorHandled=(. op) => op == Add || op == Sub,
    ~next,
  );
let next = parseAddSub;

let handleBrackets = elements => {
  let rec iter = (accum, after) =>
    switch (after) {
    | [
        UnresolvedFunction(fn, _, _),
        Unresolved(OpenBracket, i, _),
        ...after,
      ] =>
      iter(Value_BracketAccum.appendOpenBracket(accum, i, Some(fn)), after)
    | [Unresolved(OpenBracket, i, _), ...after] =>
      iter(Value_BracketAccum.appendOpenBracket(accum, i, None), after)
    | [Unresolved(CloseBracket(superscript), _, i'), ...after] =>
      switch (Value_BracketAccum.appendCloseBracket(accum)) {
      | Some((accum, func, elements)) =>
        switch (next(elements)) {
        | Ok(arg) =>
          let node =
            func
            ->Belt.Option.mapWithDefault(arg, handleFunction(arg))
            ->withSuperscript(superscript)
            ->Resolved;
          iter(Value_BracketAccum.append(accum, node), after);
        | Error(_) as e => e
        | UnknownError => Error(i')
        }
      | None => Error(i')
      }
    | [element, ...after] =>
      iter(Value_BracketAccum.append(accum, element), after)
    | [] =>
      switch (Value_BracketAccum.toList(accum)) {
      | Ok(elements) => next(elements)
      | Error(index) => Error(index)
      }
    };
  iter(Value_BracketAccum.empty, elements);
};
let next = handleBrackets;
