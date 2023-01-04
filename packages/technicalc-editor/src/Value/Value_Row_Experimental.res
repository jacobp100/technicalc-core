open Value_Builders
open Value_Types

type element = (AST.foldState<node>, (int, int))

type numericParseResult<'a> = {
  elements: ArraySlice.t<element>,
  i': int,
  value: 'a,
  continue: bool,
}

// TODO - handle mixed fractions better
// 1 1/2 = 1.5
// 1! - pass
// 1/2 pi - pass
// 1/2 ! - pass
// 1 1/2 pi - fail
// 1! 1/2 - fail
// Handle missing elements (they're almost all missing)

// Hack to get around overly aggresive inlining
@val external noinline: 'a => unit = "Object"

let parse = {
  let applyAngle = (value: Value_Types.node, angle: AST.angle): Value_Types.node =>
    switch angle {
    | Angle_Radian => OfRad(value)
    | Angle_Degree => OfDeg(value)
    | Angle_ArcMinute => OfArcMin(value)
    | Angle_ArcSecond => OfArcSec(value)
    | Angle_Gradian => OfGrad(value)
    }

  let sndU = (. (_, b)) => b

  let applyPostfixes = (elements: ArraySlice.t<element>, value) => {
    let rec iter = (accum: TechniCalcEditor.Value_Types.node, i': option<int>, elementIndex) =>
      switch ArraySlice.get(elements, elementIndex) {
      | Some((Fold_Factorial, (_, i'))) => iter(Factorial(accum), Some(i'), elementIndex + 1)
      | Some((Fold_Conj, (_, i'))) => iter(Conj(accum), Some(i'), elementIndex + 1)
      | Some((Fold_Transpose, (_, i'))) => iter(Transpose(accum), Some(i'), elementIndex + 1)
      | Some((Fold_Percent, (_, i'))) => iter(Percent(accum), Some(i'), elementIndex + 1)
      | _ =>
        switch i' {
        | Some(i') =>
          let elements = ArraySlice.sliceToEnd(elements, elementIndex)
          Some((elements, accum, i'))
        | None => None
        }
      }

    iter(value, None, 0)
  }

  let parseNumerics = {
    let rec parseLeadingNumber = (~numberState, elements: ArraySlice.t<element>, elementIndex) => {
      let nextNumberState = switch ArraySlice.get(elements, elementIndex) {
      | Some((element, range)) =>
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
          | Some((value, (_, i'))) =>
            let elements = ArraySlice.sliceToEnd(elements, elementIndex)
            Ok(Some({elements, i', value, continue: true}))
          | None =>
            let errorIndex = Value_NumberParser.range(numberState)->Belt.Option.mapU(sndU)
            Error(errorIndex)
          }
        | None => Ok(None)
        }
      }
    }

    let readFraction = (elements: ArraySlice.t<element>) =>
      switch ArraySlice.get(elements, 0) {
      | Some((Fold_Frac({num, den, superscript}), (_, i'))) =>
        let elements = ArraySlice.sliceToEnd(elements, 1)
        let fraction: node = Div(num, den)
        Some((elements, fraction, superscript, i'))
      | _ => None
      }

    let readConstants = (elements: ArraySlice.t<element>) => {
      let rec iter = (accum, elements: ArraySlice.t<element>) => {
        let next = switch ArraySlice.get(elements, 0) {
        | Some((Fold_Constant({value, superscript}), (_, i'))) =>
          Some((withSuperscript(OfEncoded(value), superscript), i'))
        | Some((Fold_ConstPi(superscript), (_, i'))) => Some((withSuperscript(Pi, superscript), i'))
        | Some((Fold_ConstE(superscript), (_, i'))) => Some((withSuperscript(E, superscript), i'))
        | Some((Fold_ImaginaryUnit(superscript), (_, i'))) =>
          Some((withSuperscript(I, superscript), i'))
        | Some((Fold_Variable({id, superscript}), (_, i'))) =>
          Some((withSuperscript(Variable(id), superscript), i'))
        | Some((Fold_X(superscript), (_, i'))) => Some((withSuperscript(X, superscript), i'))
        | _ => None
        }
        switch next {
        | Some((value, i')) =>
          let elements = ArraySlice.sliceToEnd(elements, 1)
          let (elements, value, i') = switch applyPostfixes(elements, value) {
          | Some(tuple) => tuple
          | None => (elements, value, i')
          }
          let value = switch accum {
          | None => value
          | Some((prev, _)) => Mul(prev, value)
          }
          iter(Some((value, i')), elements)
        | None =>
          switch accum {
          | Some((value, i')) => Some((elements, value, i'))
          | None => None
          }
        }
      }

      iter(None, elements)
    }

    let parseNumerics = (elements: ArraySlice.t<element>) => {
      switch parseLeadingNumber(~numberState=None, elements, 0) {
      | Ok(parseResult) =>
        // Cardinal numbers as start
        let (elements, current, continue) = switch parseResult {
        | Some({elements, i', value, continue}) => (elements, Some((value, i')), continue)
        | None => (elements, None, true)
        }

        let postfixResult = switch continue ? current : None {
        | Some((value, _)) => applyPostfixes(elements, value)
        | None => None
        }
        let (elements, current, continue) = switch postfixResult {
        | Some((elements, value, i')) => (elements, Some((value, i')), true)
        | None => (elements, current, continue)
        }

        // Handle mixed/lone fractions
        let (elements, current, continue) = switch (
          current,
          continue ? readFraction(elements) : None,
        ) {
        | (Some((value, _)), Some((elements, fraction, superscript, i')))
          if postfixResult == None =>
          // Mixed fraction - don't continue
          let value: node = Add(value, fraction)->withSuperscript(superscript)
          (elements, Some((value, i')), false)
        | (Some(_), Some(_)) =>
          // TODO - handle 1! 1/2
          assert false
        | (None, Some((elements, fraction, superscript, i'))) =>
          // Lone fraction - continue
          let value = withSuperscript(fraction, superscript)
          (elements, Some((value, i')), true)
        | (_, None) => (elements, current, continue)
        }

        // Handle constants
        let (elements, current, continue) = switch (
          current,
          continue ? readConstants(elements) : None,
        ) {
        | (Some((value, _)), Some((elements, constants, i'))) =>
          let value: node = Mul(value, constants)
          (elements, Some((value, i')), true)
        | (None, Some((elements, value, i'))) => (elements, Some((value, i')), true)
        | (_, None) => (elements, current, continue)
        }

        switch current {
        | Some((value, i')) => Ok(Some({elements, i', value, continue}))
        | None => Ok(None)
        }
      | Error(_) as e => e
      }
    }

    let rec parseAngleLine = (
      ~angleMode: option<AST.angle>,
      ~accum: option<(node, int)>,
      elements: ArraySlice.t<element>,
    ) =>
      switch parseNumerics(elements) {
      | Ok(Some({elements, value})) as res =>
        switch ArraySlice.get(elements, 0) {
        | Some((Fold_Angle(angle), (_, i'))) =>
          let angleValid = switch (angleMode, angle) {
          | (Some(Angle_Degree), Angle_ArcMinute | Angle_ArcSecond) => true
          | (Some(Angle_ArcMinute), Angle_ArcSecond) => true
          | (None, _) => true
          | _ => false
          }
          if angleValid {
            let elements = ArraySlice.sliceToEnd(elements, 1)
            let angleValue = applyAngle(value, angle)
            let accum = switch accum {
            | Some((accum, _)) => Some(((Add(angleValue, accum): node), i'))
            | None => Some((angleValue, i'))
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
            | Some((_, i')) => Error(Some(i'))
            | None => Error(None)
            }
          }
        }
      | Ok(None) =>
        switch accum {
        | Some((value, i')) => Ok(Some({elements, i', value, continue: false}))
        | None => Ok(None)
        }
      | Error(_) as e => e
      }

    let parseAngles = (elements: ArraySlice.t<element>) =>
      parseAngleLine(~angleMode=None, ~accum=None, elements)

    elements => parseAngles(elements)
  }

  let handleBinaryOperator = (~iterU, ~nextU, elements, elementIndex, op, (_, i'): (int, int)) => {
    let before = ArraySlice.slice(elements, ~offset=0, ~len=elementIndex)
    let after = ArraySlice.sliceToEnd(elements, elementIndex + 1)
    switch (iterU(. before), nextU(. after)) {
    | (Ok(before), Ok(after)) =>
      switch op {
      | AST.Fold_Add => Node.Add(before, after)->Ok
      | Fold_Sub => Sub(before, after)->Ok
      | Fold_Mul => Mul(before, after)->Ok
      | Fold_Div => Div(before, after)->Ok
      | Fold_Dot => Dot(before, after)->Ok
      | Fold_Percent => Rem(before, after)->Ok
      | _ => Error(Some(i'))
      }
    | (Error(Some(_)) as e, _)
    | (_, Error(Some(_)) as e) => e
    | (Error(None), _)
    | (_, Error(None)) =>
      Error(Some(i'))
    }
  }

  let combineValue = (a, b) =>
    switch a {
    | Some(a) => Some((Mul(a, b): node))
    | None => Some(b)
    }

  let rec parseUntilCloseBracket = (elements: ArraySlice.t<element>, startIndex) => {
    let rec iter = (~bracketLevel, i) => {
      switch ArraySlice.get(elements, i) {
      | Some((Fold_CloseBracket(superscript), _)) if bracketLevel == 1 =>
        let inner = ArraySlice.slice(elements, ~offset=startIndex, ~len=i - startIndex)
        switch parse(inner) {
        | Ok(value) => Some(Ok(value, i + 1, superscript))
        | Error(_) as e => Some(e)
        }
      | Some((Fold_OpenBracket, _)) => iter(~bracketLevel=bracketLevel + 1, i + 1)
      | Some((Fold_CloseBracket(_), _)) => iter(~bracketLevel=bracketLevel - 1, i + 1)
      | Some(_) => iter(~bracketLevel, i + 1)
      | None => None
      }
    }

    iter(~bracketLevel=1, startIndex)
  }
  and parseBracketOrRemaining = (elements: ArraySlice.t<element>, startIndex) => {
    switch ArraySlice.get(elements, startIndex) {
    | Some((Fold_OpenBracket, range)) =>
      switch parseUntilCloseBracket(elements, startIndex + 1) {
      | Some(result) => result
      | None => Error(Some(fst(range)))
      }
    | _ =>
      let remaining = ArraySlice.sliceToEnd(elements, startIndex)
      switch parse(remaining) {
      | Ok(value) => Ok((value, ArraySlice.length(elements), None))
      | Error(_) as e => e
      }
    }
  }
  and parseRemaining = (elements: ArraySlice.t<element>, initialValue) => {
    let rec iter = (accum, i) =>
      switch ArraySlice.get(elements, i) {
      | Some((Fold_OpenBracket, range)) =>
        switch parseUntilCloseBracket(elements, i + 1) {
        | Some(Ok(value, i, superscript)) =>
          let value = withSuperscript(value, superscript)
          iter(combineValue(accum, value), i)
        | Some(Error(_) as e) => e
        | None => Error(Some(fst(range)))
        }
      | Some((Fold_Function({fn, resultSuperscript}), range)) =>
        switch parseBracketOrRemaining(elements, i + 1) {
        | Ok((value, i, superscript)) if resultSuperscript == None || superscript == None =>
          let value =
            handleGenericFunction(value, fn)
            ->withSuperscript(superscript)
            ->withSuperscript(resultSuperscript)
          iter(combineValue(accum, value), i)
        | Ok(_) => Error(Some(snd(range)))
        | Error(_) as e => e
        }
      | Some((Fold_Sum({from, to}), _)) =>
        switch parseBracketOrRemaining(elements, i + 1) {
        | Ok((value, i, superscript)) =>
          let value: node = Sum({from, to, body: withSuperscript(value, superscript)})
          iter(combineValue(accum, value), i)
        | Error(_) as e => e
        }
      | Some((Fold_Product({from, to}), _)) =>
        switch parseBracketOrRemaining(elements, i + 1) {
        | Ok((value, i, superscript)) =>
          let value: node = Product({from, to, body: withSuperscript(value, superscript)})
          iter(combineValue(accum, value), i)
        | Error(_) as e => e
        }
      | Some((_, (_, i'))) => Error(Some(i'))
      | None =>
        switch accum {
        | Some(value) => Ok(value)
        | None => Error(None)
        }
      }

    iter(initialValue, 0)
  }
  and parseRow = elements => {
    noinline(parseRow)

    switch parseNumerics(elements) {
    | Ok(Some({elements, i', value, continue: false})) =>
      if ArraySlice.length(elements) == 0 {
        Ok(value)
      } else {
        Error(Some(i'))
      }
    | Ok(None) => parseRemaining(elements, None)
    | Ok(Some({elements, value, continue: true})) => parseRemaining(elements, Some(value))
    | Error(_) as e => e
    }
  }
  and parseUnary = (elements: ArraySlice.t<element>) => {
    switch ArraySlice.get(elements, 0) {
    | Some(((Fold_Add | Fold_Sub) as op, (_, i'))) =>
      let rest = ArraySlice.sliceToEnd(elements, 1)
      switch parseUnary(rest) {
      | Ok(root) =>
        let root = op == Fold_Sub ? Node.Neg(root) : root
        Ok(root)
      | Error(Some(_)) as e => e
      | Error(None) => Error(Some(i'))
      }
    | _ => parseRow(elements)
    }
  }
  // It's hard to unify parsing mul/div and add/sub since one has prefixes
  // and the other postfixes, and it would be more effort to make a generic
  // implementation that handled both
  and parseMulDiv = (elements: ArraySlice.t<element>) => {
    let iterU = (. elements) => parseMulDiv(elements)
    let nextU = (. elements) => parseUnary(elements)

    let rec iter = (~bracketLevel, ~inPostFixPosition, elementIndex) =>
      switch ArraySlice.get(elements, elementIndex) {
      | Some((Fold_OpenBracket, _)) =>
        iter(~bracketLevel=bracketLevel + 1, ~inPostFixPosition, elementIndex - 1)
      | Some((Fold_CloseBracket(_), _)) =>
        iter(~bracketLevel=bracketLevel - 1, ~inPostFixPosition, elementIndex - 1)
      | Some(((Fold_Mul | Fold_Div | Fold_Dot) as op, range)) if bracketLevel == 0 =>
        // These operators are never post fixes
        handleBinaryOperator(~iterU, ~nextU, elements, elementIndex, op, range)
      | Some((Fold_Percent as op, range)) if bracketLevel == 0 && !inPostFixPosition =>
        handleBinaryOperator(~iterU, ~nextU, elements, elementIndex, op, range)
      | Some(_) => iter(~bracketLevel, ~inPostFixPosition=false, elementIndex - 1)
      | None => nextU(. elements)
      }

    iter(~bracketLevel=0, ~inPostFixPosition=true, ArraySlice.length(elements) - 1)
  }
  and parseAddSub = (elements: ArraySlice.t<element>) => {
    let iterU = (. elements) => parseAddSub(elements)
    let nextU = (. elements) => parseMulDiv(elements)

    let rec iter = (~bracketLevel, ~inPrefixPosition, accum, elementIndex) =>
      switch ArraySlice.get(elements, elementIndex) {
      | Some((Fold_OpenBracket, _)) =>
        iter(~bracketLevel=bracketLevel + 1, ~inPrefixPosition, accum, elementIndex + 1)
      | Some((Fold_CloseBracket(_), _)) =>
        iter(~bracketLevel=bracketLevel - 1, ~inPrefixPosition, accum, elementIndex + 1)
      | Some(((Fold_Add | Fold_Sub) as op, range)) if bracketLevel == 0 =>
        // Both + and - can be prefixes
        let nextAccum = !inPrefixPosition ? Some((elementIndex, op, range)) : accum
        iter(~bracketLevel, ~inPrefixPosition=true, nextAccum, elementIndex + 1)
      | Some(_) => iter(~bracketLevel, ~inPrefixPosition=false, accum, elementIndex + 1)
      | None =>
        switch accum {
        | Some((elementIndex, op, range)) =>
          handleBinaryOperator(~iterU, ~nextU, elements, elementIndex, op, range)
        | None => nextU(. elements)
        }
      }

    iter(~bracketLevel=0, ~inPrefixPosition=true, None, 0)
  }
  and parse = elements => parseAddSub(elements)

  let parse = elements => ArraySlice.ofArray(elements)->parse
  parse
}
