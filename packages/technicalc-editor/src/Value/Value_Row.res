open Value_Builders
open Value_Types

type parseResult =
  | Ok(node)
  | Error(int)
  | UnknownError

type angleState = option<(Value_Types.node, AST.angle)>

type openBracket = {
  startElementIndex: int,
  endElementIndex: int,
  fn: option<funcitionLike>,
  i': int,
}

// Rather than %%private everything (which currently has syntax), just wrap in a block
// This is so more optimisations can be applied
// Less stuff needs exported, so inlining can be more aggressive
let parse = {
  let applyAngle = (value: Value_Types.node, angle: AST.angle): Value_Types.node =>
    switch angle {
    | Angle_Radian => OfRad(value)
    | Angle_Degree => OfDeg(value)
    | Angle_ArcMinute => OfArcMin(value)
    | Angle_ArcSecond => OfArcSec(value)
    | Angle_Gradian => OfGrad(value)
    }

  let rec applyPostfixes = (elements, accum: TechniCalcEditor.Value_Types.node, elementIndex) => {
    let nextElement = ArraySlice.get(elements, elementIndex + 1)
    let nextAccum: option<TechniCalcEditor.Value_Types.node> = switch nextElement {
    | Some(Unresolved(Fold_UnitConversion({fromUnits, toUnits}), _, _)) =>
      Some(Convert({body: accum, fromUnits: fromUnits, toUnits: toUnits}))
    | Some(Unresolved(Fold_Factorial, _, _)) => Some(Factorial(accum))
    | Some(Unresolved(Fold_Conj, _, _)) => Some(Conj(accum))
    | Some(Unresolved(Fold_Percent, _, _)) => Some(Percent(accum))
    | _ => None
    }
    switch nextAccum {
    | Some(nextAccum) => applyPostfixes(elements, nextAccum, elementIndex + 1)
    | None => (accum, elementIndex)
    }
  }

  let parsePostfixesAndRest = elements => {
    let rec iter = (current, elementIndex) =>
      switch ArraySlice.get(elements, elementIndex) {
      | Some(Resolved(value)) =>
        let (value, elementIndex) = applyPostfixes(elements, value, elementIndex)
        let value = switch current {
        | Some(a) => Node.Mul(a, value)
        | None => value
        }
        let (value, elementIndex) = switch ArraySlice.get(elements, elementIndex + 1) {
        | Some(Unresolved(Fold_Angle(angle), _, _)) => (applyAngle(value, angle), elementIndex + 1)
        | _ => (value, elementIndex)
        }
        iter(Some(value), elementIndex + 1)
      | Some(UnresolvedFunction(_, _, i') | Unresolved(_, _, i')) => Error(i')
      | None =>
        switch current {
        | Some(v) => Ok(v)
        | None => UnknownError
        }
      }
    iter(None, 0)
  }
  let next = parsePostfixesAndRest

  let parseNumbers = elements => {
    let next' = (numberState, angleState: angleState, elementIndex) => {
      let prependNode = switch (Value_NumberParser.toNode(numberState), angleState) {
      | (None, Some((angleAccum, _))) => Some(Resolved(angleAccum))
      | (Some(number), None) => Some(Resolved(number))
      | _ => None
      }
      let nextElements = ArraySlice.sliceToEnd(elements, elementIndex)
      let nextElements = switch prependNode {
      | Some(prependNode) =>
        ArraySlice.toArray(nextElements)->Belt.Array.concat([prependNode], _)->ArraySlice.ofArray
      | None => nextElements
      }
      next(nextElements)
    }

    let rec iter = (numberState, angleState, elementIndex) => {
      let element = ArraySlice.get(elements, elementIndex)

      switch element {
      | Some(Unresolved(Fold_Angle(angle), i, _)) =>
        let number = switch Value_NumberParser.toNode(numberState) {
        | Some(number) => Some(applyAngle(number, angle))
        | None => None
        }
        switch (number, (angleState: angleState), angle) {
        | (Some(number), None, _) =>
          iter(Value_NumberParser.empty, Some((number, angle)), elementIndex + 1)
        | (Some(number), Some((angleAccum, Angle_Degree)), Angle_ArcMinute | Angle_ArcSecond)
        | (Some(number), Some((angleAccum, Angle_ArcMinute)), Angle_ArcSecond) =>
          iter(Value_NumberParser.empty, Some((Add(angleAccum, number), angle)), elementIndex + 1)
        | _ => Error(i)
        }
      | _ =>
        let nextNumberState = switch element {
        | Some(Unresolved(element, _, _)) => Value_NumberParser.reduce(numberState, element)
        | _ => None
        }
        switch nextNumberState {
        | Some(nextNumberState) => iter(nextNumberState, angleState, elementIndex + 1)
        | None => next'(numberState, angleState, elementIndex)
        }
      }
    }

    iter(Value_NumberParser.empty, None, 0)
  }
  let next = parseNumbers

  let rec parseUnary = elements =>
    switch ArraySlice.get(elements, 0) {
    | Some(Unresolved(Fold_Operator((Op_Add | Op_Sub) as op), _, i')) =>
      let rest = ArraySlice.sliceToEnd(elements, 1)
      switch parseUnary(rest) {
      | Ok(root) =>
        let root = op == Op_Sub ? Node.Neg(root) : root
        Ok(root)
      | Error(_) as e => e
      | UnknownError => Error(i')
      }
    | _ => next(elements)
    }
  let next = parseUnary

  let rec parseParenFreeFunctions = elements => {
    let rec iter = elementIndex =>
      switch ArraySlice.get(elements, elementIndex) {
      | Some(UnresolvedFunction(fn, _, i')) =>
        let after = ArraySlice.sliceToEnd(elements, elementIndex + 1)
        switch parseParenFreeFunctions(after) {
        | Ok(arg) =>
          let before =
            ArraySlice.slice(elements, ~offset=0, ~len=elementIndex)
            ->ArraySlice.toArray
            ->Belt.Array.concat(_, [Resolved(handleFunction(fn, arg))])
            ->ArraySlice.ofArray
          next(before)
        | Error(_) as e => e
        | UnknownError => Error(i')
        }
      | Some(_) => iter(elementIndex + 1)
      | None => next(elements)
      }
    iter(0)
  }
  let next = parseParenFreeFunctions

  let rec binaryOperatorParserU = (~operatorHandled, ~next, elements) => {
    let next' = current =>
      switch current {
      | Some((elementIndex, op, i')) =>
        let before = ArraySlice.slice(elements, ~offset=0, ~len=elementIndex)
        let after = ArraySlice.sliceToEnd(elements, elementIndex + 1)
        switch (binaryOperatorParserU(~operatorHandled, ~next, before), next(. after)) {
        | (Ok(before), Ok(after)) => Ok(handleOp(op, before, after))
        | (Error(_) as e, _)
        | (_, Error(_) as e) => e
        | (UnknownError, _)
        | (_, UnknownError) =>
          Error(i')
        }
      | None => next(. elements)
      }

    let rec iter = (~unaryPosition, current, elementIndex) =>
      switch ArraySlice.get(elements, elementIndex) {
      | Some(Unresolved(Fold_Operator(op), _, i')) =>
        let nextAccum =
          !unaryPosition && operatorHandled(. op) ? Some((elementIndex, op, i')) : current
        iter(~unaryPosition=true, nextAccum, elementIndex + 1)
      | Some(_) => iter(~unaryPosition=false, current, elementIndex + 1)
      | None => next'(current)
      }

    iter(~unaryPosition=true, None, 0)
  }

  let mulDivOperatorHandled = (. op) => op == AST.Op_Mul || op == Op_Div || op == Op_Dot
  let parseMulDiv = elements =>
    binaryOperatorParserU(
      ~operatorHandled=mulDivOperatorHandled,
      ~next=(. elements) => next(elements),
      elements,
    )
  let next = parseMulDiv

  let addSubOperatorHandled = (. op) => op == AST.Op_Add || op == Op_Sub
  let parseAddSub = elements =>
    binaryOperatorParserU(
      ~operatorHandled=addSubOperatorHandled,
      ~next=(. elements) => next(elements),
      elements,
    )
  let next = parseAddSub

  let handleBrackets = elements => {
    let rec iter = (elements, openBracketStack, elementIndex) =>
      switch ArraySlice.get(elements, elementIndex) {
      | Some(Unresolved(Fold_OpenBracket, _, i')) =>
        let fnIndex = elementIndex - 1
        let fn = switch ArraySlice.get(elements, fnIndex) {
        | Some(UnresolvedFunction(fn, _, _)) => Some(fn)
        | _ => None
        }
        let startElementIndex = fn != None ? fnIndex : elementIndex
        let endElementIndex = elementIndex + 1
        let openBracket = {
          startElementIndex: startElementIndex,
          endElementIndex: endElementIndex,
          fn: fn,
          i': i',
        }
        iter(elements, list{openBracket, ...openBracketStack}, endElementIndex)
      | Some(Unresolved(Fold_CloseBracket(superscript), _, i')) =>
        switch openBracketStack {
        | list{{startElementIndex, endElementIndex, fn}, ...openBracketStack} =>
          let innerElements = ArraySlice.slice(
            elements,
            ~offset=endElementIndex,
            ~len=elementIndex - endElementIndex,
          )
          switch next(innerElements) {
          | Ok(arg) =>
            let node =
              switch fn {
              | Some(fn) => handleFunction(fn, arg)
              | None => arg
              }
              ->withSuperscript(_, superscript)
              ->Resolved
            let before =
              ArraySlice.slice(elements, ~offset=0, ~len=startElementIndex)->ArraySlice.toArray
            let after = ArraySlice.sliceToEnd(elements, elementIndex + 1)->ArraySlice.toArray
            let nextElements = Belt.Array.concatMany([before, [node], after])->ArraySlice.ofArray
            iter(nextElements, openBracketStack, startElementIndex + 1)
          | Error(_) as e => e
          | UnknownError => Error(i')
          }
        | list{} => Error(i')
        }
      | Some(_) => iter(elements, openBracketStack, elementIndex + 1)
      | None =>
        switch openBracketStack {
        | list{} => next(elements)
        | list{{i'}, ..._} => Error(i')
        }
      }
    iter(elements, list{}, 0)
  }
  let next = handleBrackets

  elements => next(ArraySlice.ofArray(elements))
}
