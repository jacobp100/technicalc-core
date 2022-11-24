open Value_Builders
open Value_Types

type openBracket = {
  startElementIndex: int,
  endElementIndex: int,
  fn: option<funcitionLike>,
  range: (int, int),
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
    | Some(Unresolved(Fold_UnitConversion({fromUnits, toUnits}), _)) =>
      Some(Convert({body: accum, fromUnits, toUnits}))
    | Some(Unresolved(Fold_Factorial, _)) => Some(Factorial(accum))
    | Some(Unresolved(Fold_Conj, _)) => Some(Conj(accum))
    | Some(Unresolved(Fold_Transpose, _)) => Some(Transpose(accum))
    | Some(Unresolved(Fold_Percent, _)) => Some(Percent(accum))
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
      | Some(Resolved(value, _)) =>
        let (value, elementIndex) = applyPostfixes(elements, value, elementIndex)
        let value = switch current {
        | Some(a) => Node.Mul(a, value)
        | None => value
        }
        iter(Some(value), elementIndex + 1)
      | Some(UnresolvedFunction(_, (_, i')) | Unresolved(_, (_, i'))) => Error(Some(i'))
      | None =>
        switch current {
        | Some(v) => Ok(v)
        | None => Error(None)
        }
      }
    iter(None, 0)
  }
  let next = parsePostfixesAndRest

  let parseSimpleNumerics = {
    let rec parseLeadingNumber = (~numberState, elements, elementIndex) => {
      let nextNumberState = switch ArraySlice.get(elements, elementIndex) {
      | Some(Unresolved(element, range)) =>
        Belt.Option.getWithDefault(
          numberState,
          Value_NumberParser.empty,
        )->Value_NumberParser.reduce(element, range)
      | _ => None
      }

      switch nextNumberState {
      | Some(Ok(numberState)) =>
        parseLeadingNumber(~numberState=Some(numberState), elements, elementIndex + 1)
      | Some(Error(_) as e) => e
      | None =>
        switch numberState {
        | Some(numberState) =>
          switch Value_NumberParser.toNode(numberState) {
          | Some((number, range)) =>
            let elements = ArraySlice.sliceToEnd(elements, elementIndex)
            Ok(Some((elements, number, range, true)))
          | None =>
            let errorIndex = Value_NumberParser.range(numberState)->Belt.Option.map(snd)
            Error(errorIndex)
          }
        | None => Ok(None)
        }
      }
    }

    let parseMixedFraction = elements => {
      switch parseLeadingNumber(~numberState=None, elements, 0) {
      | Ok(Some(elements, number, (i, _), _)) as res =>
        switch ArraySlice.get(elements, 0) {
        | Some(Resolved(Div(_, _) as fraction, (_, i'))) =>
          let elements = ArraySlice.sliceToEnd(elements, 1)
          let number: node = Add(number, fraction)
          Ok(Some((elements, number, (i, i'), false)))
        | _ => res
        }
      | Ok(None) => Ok(None)
      | Error(_) as e => e
      }
    }

    let rec parseAngleLine = (~angleMode: option<AST.angle>, ~accum, elements) =>
      switch parseMixedFraction(elements) {
      | Ok(Some((elements, number, (i, _), _))) as res =>
        switch ArraySlice.get(elements, 0) {
        | Some(Unresolved(Fold_Angle(angle), (_, i'))) =>
          let angleValid = switch (angleMode, angle) {
          | (Some(Angle_Degree), Angle_ArcMinute | Angle_ArcSecond) => true
          | (Some(Angle_ArcMinute), Angle_ArcSecond) => true
          | (None, _) => true
          | _ => false
          }
          if angleValid {
            let elements = ArraySlice.sliceToEnd(elements, 1)
            let angleValue = applyAngle(number, angle)
            let accum = switch accum {
            | Some((accum, (i, _))) => Some((Add(angleValue, accum): node), (i, i'))
            | None => Some((angleValue, (i, i')))
            }
            parseAngleLine(~angleMode=Some(angle), ~accum, elements)
          } else {
            Error(Some(i'))
          }
        | _ =>
          switch angleMode {
          | None => res
          | _ =>
            switch accum {
            | Some(_, (_, i')) => Error(Some(i'))
            | None => Error(None)
            }
          }
        }
      | Ok(None) =>
        switch accum {
        | Some((accum, range)) => Ok(Some(elements, accum, range, false))
        | None => Ok(None)
        }
      | Error(_) as e => e
      }

    elements =>
      switch parseAngleLine(~angleMode=None, ~accum=None, elements) {
      | Ok(Some((elements, number, (_, i'), continue))) =>
        if ArraySlice.length(elements) == 0 {
          Ok(number)
        } else if continue {
          let elements =
            Belt.Array.concat(
              [Resolved(number, Obj.magic(0))],
              ArraySlice.toArray(elements),
            )->ArraySlice.ofArray
          next(elements)
        } else {
          Error(Some(i'))
        }
      | Ok(None) => next(elements)
      | Error(_) as e => e
      }
  }
  let next = parseSimpleNumerics

  let rec parseUnary = elements =>
    switch ArraySlice.get(elements, 0) {
    | Some(Unresolved(Fold_Operator((Op_Add | Op_Sub) as op), (_, i'))) =>
      let rest = ArraySlice.sliceToEnd(elements, 1)
      switch parseUnary(rest) {
      | Ok(root) =>
        let root = op == Op_Sub ? Node.Neg(root) : root
        Ok(root)
      | Error(Some(_)) as e => e
      | Error(None) => Error(Some(i'))
      }
    | _ => next(elements)
    }
  let next = parseUnary

  let rec parseParenFreeFunctions = elements => {
    let rec iter = elementIndex =>
      switch ArraySlice.get(elements, elementIndex) {
      | Some(UnresolvedFunction(fn, (_, i') as range)) =>
        let after = ArraySlice.sliceToEnd(elements, elementIndex + 1)
        switch parseParenFreeFunctions(after) {
        | Ok(arg) =>
          let before =
            ArraySlice.slice(elements, ~offset=0, ~len=elementIndex)
            ->ArraySlice.toArray
            ->Belt.Array.concat(_, [Resolved(handleFunction(fn, arg), range)])
            ->ArraySlice.ofArray
          next(before)
        | Error(Some(_)) as e => e
        | Error(None) => Error(Some(i'))
        }
      | Some(_) => iter(elementIndex + 1)
      | None => next(elements)
      }
    iter(0)
  }
  let next = parseParenFreeFunctions

  let rec binaryOperatorParserU = (~operatorHandledU, ~nextU, elements) => {
    let next' = current =>
      switch current {
      | Some((elementIndex, op, i')) =>
        let before = ArraySlice.slice(elements, ~offset=0, ~len=elementIndex)
        let after = ArraySlice.sliceToEnd(elements, elementIndex + 1)
        switch (binaryOperatorParserU(~operatorHandledU, ~nextU, before), nextU(. after)) {
        | (Ok(before), Ok(after)) => Ok(handleOp(op, before, after))
        | (Error(Some(_)) as e, _)
        | (_, Error(Some(_)) as e) => e
        | (Error(None), _)
        | (_, Error(None)) =>
          Error(Some(i'))
        }
      | None => nextU(. elements)
      }

    let rec iter = (~unaryPosition, current, elementIndex) =>
      switch ArraySlice.get(elements, elementIndex) {
      | Some(Unresolved(Fold_Operator(op), (_, i'))) =>
        let nextAccum =
          !unaryPosition && operatorHandledU(. op) ? Some((elementIndex, op, i')) : current
        iter(~unaryPosition=true, nextAccum, elementIndex + 1)
      | Some(_) => iter(~unaryPosition=false, current, elementIndex + 1)
      | None => next'(current)
      }

    iter(~unaryPosition=true, None, 0)
  }

  let parseMulDiv = {
    let nextU = (. elements) => next(elements)
    let operatorHandledU = (. op) =>
      op == AST.Op_Mul || op == Op_Div || op == Op_Dot || op == Op_Rem
    elements => binaryOperatorParserU(~operatorHandledU, ~nextU, elements)
  }
  let next = parseMulDiv

  let parseAddSub = {
    let operatorHandledU = (. op) => op == AST.Op_Add || op == Op_Sub
    let nextU = (. elements) => next(elements)
    elements => binaryOperatorParserU(~operatorHandledU, ~nextU, elements)
  }
  let next = parseAddSub

  let handleBrackets = elements => {
    let rec iter = (elements, openBracketStack, elementIndex) =>
      switch ArraySlice.get(elements, elementIndex) {
      | Some(Unresolved(Fold_OpenBracket, range)) =>
        let fnIndex = elementIndex - 1
        let fn = switch ArraySlice.get(elements, fnIndex) {
        | Some(UnresolvedFunction(fn, (_, _))) => Some(fn)
        | _ => None
        }
        let startElementIndex = fn != None ? fnIndex : elementIndex
        let endElementIndex = elementIndex + 1
        let openBracket = {
          startElementIndex,
          endElementIndex,
          fn,
          range,
        }
        iter(elements, list{openBracket, ...openBracketStack}, endElementIndex)
      | Some(Unresolved(Fold_CloseBracket(superscript), (_, i'))) =>
        switch openBracketStack {
        | list{{startElementIndex, endElementIndex, fn, range: (i, _)}, ...openBracketStack} =>
          let innerElements = ArraySlice.slice(
            elements,
            ~offset=endElementIndex,
            ~len=elementIndex - endElementIndex,
          )
          switch next(innerElements) {
          | Ok(arg) =>
            let node = switch fn {
            | Some(fn) => handleFunction(fn, arg)
            | None => arg
            }->withSuperscript(_, superscript)
            let node = Resolved(node, (i, i'))
            let before =
              ArraySlice.slice(elements, ~offset=0, ~len=startElementIndex)->ArraySlice.toArray
            let after = ArraySlice.sliceToEnd(elements, elementIndex + 1)->ArraySlice.toArray
            let nextElements = Belt.Array.concatMany([before, [node], after])->ArraySlice.ofArray
            iter(nextElements, openBracketStack, startElementIndex + 1)
          | Error(Some(_)) as e => e
          | Error(None) => Error(Some(i'))
          }
        | list{} => Error(Some(i'))
        }
      | Some(_) => iter(elements, openBracketStack, elementIndex + 1)
      | None =>
        switch openBracketStack {
        | list{} => next(elements)
        | list{{range: (_, i')}, ..._} => Error(Some(i'))
        }
      }
    iter(elements, list{}, 0)
  }
  let next = handleBrackets

  elements => next(ArraySlice.ofArray(elements))
}
