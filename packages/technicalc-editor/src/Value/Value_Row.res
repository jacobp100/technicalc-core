open Value_Builders
open Value_Types

%%private(
  let applyAngle = (value: Value_Types.node, angle: AST_ReduceMap.angle): Value_Types.node =>
    switch angle {
    | Radian => OfRad(value)
    | Degree => OfDeg(value)
    | ArcMinute => OfArcMin(value)
    | ArcSecond => OfArcSec(value)
    | Gradian => OfGrad(value)
    }
)

type parseResult =
  | Ok(node)
  | Error(int)
  | UnknownError

let rec parseRest = (~current=None, elements) =>
  switch (current, elements) {
  | (_, list{Resolved(next), ...rest}) =>
    let value = switch current {
    | Some(a) => Node.Mul(a, next)
    | None => next
    }
    let (value, rest) = switch rest {
    | list{Unresolved(Angle(angle), _, _), ...rest} => (applyAngle(value, angle), rest)
    | _ => (value, rest)
    }
    parseRest(~current=Some(value), rest)
  | (Some(a), list{Unresolved(Percent, _, _)}) => Ok(Percent(a))
  | (_, list{UnresolvedFunction(_, _, i') | Unresolved(_, _, i'), ..._}) => Error(i')
  | (Some(v), list{}) => Ok(v)
  | (None, list{}) => UnknownError
  }
let next = parseRest

let rec parsePostfixes = elements =>
  switch elements {
  | list{Resolved(next), Unresolved(UnitConversion({fromUnits, toUnits}), _, _), ...rest} =>
    parsePostfixes(list{
      Resolved(Convert({body: next, fromUnits: fromUnits, toUnits: toUnits})),
      ...rest,
    })
  | list{Resolved(next), Unresolved(Factorial, _, _), ...rest} =>
    parsePostfixes(list{Resolved(Factorial(next)), ...rest})
  | list{Resolved(next), Unresolved(Conj, _, _), ...rest} =>
    parsePostfixes(list{Resolved(Conj(next)), ...rest})
  | _ => next(elements)
  }
let next = parsePostfixes

type angleState = option<(Value_Types.node, AST_ReduceMap.angle)>

let parseNumbers = elements => {
  let next' = (numberState, angleState: angleState, rest) =>
    switch (Value_NumberParser.toNode(numberState), angleState) {
    | (None, Some((angleAccum, _))) => next(list{Resolved(angleAccum), ...rest})
    | (Some(number), None) => next(list{Resolved(number), ...rest})
    | _ => next(elements)
    }
  let rec iter = (numberState, angleState: angleState, rest) =>
    switch rest {
    | list{Unresolved(Angle(angle), i, _), ...rest} =>
      let number = switch Value_NumberParser.toNode(numberState) {
      | Some(number) => Some(applyAngle(number, angle))
      | None => None
      }
      switch (number, angleState, angle) {
      | (Some(number), None, _) => iter(Value_NumberParser.empty, Some((number, angle)), rest)
      | (Some(number), Some((angleAccum, Degree)), ArcMinute | ArcSecond)
      | (Some(number), Some((angleAccum, ArcMinute)), ArcSecond) =>
        iter(Value_NumberParser.empty, Some((Add(angleAccum, number), angle)), rest)
      | _ => Error(i)
      }
    | list{Unresolved(element, _, _), ...after} =>
      switch Value_NumberParser.reduce(numberState, element) {
      | Some(s) => iter(s, angleState, after)
      | None => next'(numberState, angleState, rest)
      }
    | _ => next'(numberState, angleState, rest)
    }
  iter(Value_NumberParser.empty, None, elements)
}
let next = parseNumbers

let rec parseUnary = elements =>
  switch elements {
  | list{Unresolved(Operator((Add | Sub) as op), _, i'), ...rest} =>
    switch parseUnary(rest) {
    | Ok(root) =>
      let root = op == Sub ? Node.Neg(root) : root
      Ok(root)
    | Error(_) as e => e
    | UnknownError => Error(i')
    }
  | _ => next(elements)
  }
let next = parseUnary

let rec parseParenFreeFunctions = elements => {
  let rec iter = (after, index) =>
    switch after {
    | list{UnresolvedFunction(fn, _, i'), ...after} =>
      switch parseParenFreeFunctions(after) {
      | Ok(arg) =>
        ListUtil.takeUpto(elements, index)
        ->Belt.List.concat(list{Resolved(handleFunction(arg, fn))})
        ->next
      | Error(_) as e => e
      | UnknownError => Error(i')
      }
    | list{_, ...after} => iter(after, index + 1)
    | list{} => next(elements)
    }
  iter(elements, 0)
}
let next = parseParenFreeFunctions

let binaryOperatorParser = (~operatorHandled, ~next) => {
  let rec iter = (unaryPosition, current, after, index) =>
    switch after {
    | list{Unresolved(Operator(op), _, i'), ...after} =>
      let nextAccum =
        !unaryPosition && operatorHandled(. op) ? Some((after, index, op, i')) : current
      iter(true, nextAccum, after, index + 1)
    | list{_, ...after} => iter(false, current, after, index + 1)
    | list{} => current
    }
  let rec inner = elements =>
    switch iter(true, None, elements, 0) {
    | Some((after, index, op, i')) =>
      switch (ListUtil.takeUpto(elements, index)->inner, next(after)) {
      | (Ok(before), Ok(after)) => Ok(handleOp(op, before, after))
      | (Error(_) as e, _)
      | (_, Error(_) as e) => e
      | (UnknownError, _)
      | (_, UnknownError) =>
        Error(i')
      }
    | None => next(elements)
    }
  inner
}

let parseMulDiv = binaryOperatorParser(
  ~operatorHandled=(. op) => op == Mul || (op == Div || op == Dot),
  ~next,
)
let next = parseMulDiv

let parseAddSub = binaryOperatorParser(~operatorHandled=(. op) => op == Add || op == Sub, ~next)
let next = parseAddSub

let handleBrackets = elements => {
  let rec iter = (accum, after) =>
    switch after {
    | list{UnresolvedFunction(fn, _, _), Unresolved(OpenBracket, i, _), ...after} =>
      iter(Value_BracketAccum.appendOpenBracket(accum, i, Some(fn)), after)
    | list{Unresolved(OpenBracket, i, _), ...after} =>
      iter(Value_BracketAccum.appendOpenBracket(accum, i, None), after)
    | list{Unresolved(CloseBracket(superscript), _, i'), ...after} =>
      switch Value_BracketAccum.appendCloseBracket(accum) {
      | Some((accum, func, elements)) =>
        switch next(elements) {
        | Ok(arg) =>
          let node =
            func
            ->Belt.Option.mapWithDefault(arg, handleFunction(arg))
            ->withSuperscript(superscript)
            ->Resolved
          iter(Value_BracketAccum.append(accum, node), after)
        | Error(_) as e => e
        | UnknownError => Error(i')
        }
      | None => Error(i')
      }
    | list{element, ...after} => iter(Value_BracketAccum.append(accum, element), after)
    | list{} =>
      switch Value_BracketAccum.toList(accum) {
      | Ok(elements) => next(elements)
      | Error(index) => Error(index)
      }
    }
  iter(Value_BracketAccum.empty, elements)
}
let next = handleBrackets
