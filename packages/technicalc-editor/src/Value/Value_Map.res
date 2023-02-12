open Value_Builders
open Value_Types

type elements = ArraySlice.t<(AST.foldState<node>, (int, int))>

// TODO - handle mixed fractions better
// 1 1/2 = 1.5
// 1! - pass
// 1/2 pi - pass
// 1/2 ! - pass
// 1 1/2 pi - fail
// 1! 1/2 - fail
// Postfixes on fractions

// Hack to get around overly aggresive inlining
external noinline: 'a => unit = "%identity"

let parse = {
  let angleIsSmaller = (~angleRequirement, angle) =>
    switch (angleRequirement, angle) {
    | (None, _)
    | (Some(AST.Angle_Degree), AST.Angle_ArcMinute | Angle_ArcSecond)
    | (Some(Angle_ArcMinute), Angle_ArcSecond) => true
    | _ => false
    }

  let handleElement = (element: AST.foldState<node>) =>
    switch element {
    | Fold_Abs({arg, superscript}) => Some(withSuperscript(Abs(arg), superscript))
    | Fold_Ceil({arg, superscript}) => Some(withSuperscript(Ceil(arg), superscript))
    | Fold_Constant({value, superscript}) => Some(withSuperscript(OfEncoded(value), superscript))
    | Fold_ConstE(superscript) => Some(withSuperscript(E, superscript))
    | Fold_ConstPi(superscript) => Some(withSuperscript(Pi, superscript))
    | Fold_Differential({at, body}) => Some(Differential({at, body}))
    | Fold_Floor({arg, superscript}) => Some(withSuperscript(Floor(arg), superscript))
    | Fold_Gcd({a, b, superscript}) => Some(withSuperscript(Gcd(a, b), superscript))
    | Fold_ImaginaryUnit(superscript) => Some(withSuperscript(I, superscript))
    | Fold_Integral({from, to, body}) => Some(Integral({from, to, body}))
    | Fold_Lcm({a, b, superscript}) => Some(withSuperscript(Lcm(a, b), superscript))
    | Fold_Max({a, b, superscript}) => Some(withSuperscript(Max(a, b), superscript))
    | Fold_Min({a, b, superscript}) => Some(withSuperscript(Min(a, b), superscript))
    | Fold_NCR({n, r}) => Some(NCR(n, r))
    | Fold_NPR({n, r}) => Some(NPR(n, r))
    | Fold_NRoot({degree, radicand, superscript}) =>
      Some(withSuperscript(Pow(radicand, Div(One, degree)), superscript))
    | Fold_Rand(superscript) => Some(withSuperscript(Rand, superscript))
    | Fold_RandInt({a, b, superscript}) => Some(withSuperscript(RandInt(a, b), superscript))
    | Fold_Round({arg, superscript}) => Some(withSuperscript(Round(arg), superscript))
    | Fold_Sqrt({radicand, superscript}) => Some(withSuperscript(Sqrt(radicand), superscript))
    | Fold_Table({elements, numColumns: 1, superscript}) =>
      Some(withSuperscript(Vector(elements), superscript))
    | Fold_Table({elements, numRows, numColumns, superscript}) =>
      Some(withSuperscript(Matrix({numRows, numColumns, elements}), superscript))
    | Fold_Variable({id, superscript}) => Some(withSuperscript(Variable(id), superscript))
    | Fold_IterationX(superscript) => Some(withSuperscript(X, superscript))
    | Fold_XUnit(superscript) => Some(withSuperscript(XUnit, superscript))
    | Fold_YUnit(superscript) => Some(withSuperscript(YUnit, superscript))
    | Fold_ZUnit(superscript) => Some(withSuperscript(ZUnit, superscript))

    // Handled in Value
    | Fold_Placeholder(_) => None
    // Handled in number parser
    | Fold_Base(_)
    | Fold_DecimalSeparator
    | Fold_Magnitude(_)
    | Fold_Digit(_) =>
      None
    // Handled in Value_Map
    | Fold_Add
    | Fold_Angle(_)
    | Fold_CloseBracket(_)
    | Fold_Conj
    | Fold_Div
    | Fold_Dot
    | Fold_Factorial
    | Fold_Frac(_)
    | Fold_Function(_)
    | Fold_Mul
    | Fold_OpenBracket
    | Fold_Percent
    | Fold_Sub
    | Fold_Transpose
    | Fold_Unit(_) =>
      None
    }

  let applyPostFixes = (elements: elements, startIndex, value) => {
    let rec iter = (accum: TechniCalcEditor.Value_Types.node, i) =>
      switch ArraySlice.get(elements, i) {
      | Some((Fold_Factorial, _)) => iter(Factorial(accum), i + 1)
      | Some((Fold_Conj, _)) => iter(Conj(accum), i + 1)
      | Some((Fold_Transpose, _)) => iter(Transpose(accum), i + 1)
      | Some((Fold_Percent, _)) => iter(Percent(accum), i + 1)
      | Some((Fold_Unit({prefix, name, superscript}), _)) =>
        // TODO - handle consecutive units - in same place as angles
        let power = switch superscript {
        | Some({superscriptBody}) => superscriptBody
        | None => OfInt(1)
        }
        iter(Measure(accum, [{prefix, name, power}]), i + 1)
      | _ => (i, accum)
      }

    iter(value, startIndex)
  }

  let applyBinaryOperator = (
    ~angleRequirement,
    ~rangeEnd,
    ~iterU,
    ~nextU,
    elements,
    i,
    op,
    (_, r'): (int, int),
  ) => {
    let before = ArraySlice.slice(elements, ~offset=0, ~len=i)
    let after = ArraySlice.sliceToEnd(elements, i + 1)
    switch (
      iterU(. angleRequirement, rangeEnd, before),
      nextU(. angleRequirement, rangeEnd, after),
    ) {
    | (Ok(before), Ok(after)) =>
      switch op {
      | AST.Fold_Add => Node.Add(before, after)->Ok
      | Fold_Sub => Sub(before, after)->Ok
      | Fold_Mul => Mul(before, after)->Ok
      | Fold_Div => Div(before, after)->Ok
      | Fold_Dot => Dot(before, after)->Ok
      | Fold_Percent => Rem(before, after)->Ok
      | _ => Error(r')
      }
    | (Error(_) as e, _)
    | (_, Error(_) as e) => e
    }
  }

  let readNumber = (elements: elements) => {
    let rec iter = (~numberState, i) => {
      let nextNumberState = switch ArraySlice.get(elements, i) {
      | Some((element, range)) =>
        Belt.Option.getWithDefault(
          numberState,
          Value_NumberParser.empty,
        )->Value_NumberParser.reduce(element, range)
      | _ => None
      }

      switch nextNumberState {
      | Some(Ok(numberState)) => iter(~numberState=Some(numberState), i + 1)
      | Some(Error(_) as e) => e
      | None =>
        switch numberState {
        | Some(numberState) =>
          switch Value_NumberParser.toNode(numberState) {
          | Some(value) =>
            let elements = ArraySlice.sliceToEnd(elements, i)
            Ok((elements, Some(value)))
          | None =>
            switch Value_NumberParser.range(numberState) {
            | Some((_, r')) => Error(r')
            | None => Ok((elements, None)) // Pretty sure this case never happens
            }
          }
        | None => Ok((elements, None))
        }
      }
    }

    iter(~numberState=None, 0)
  }

  let readFraction = (elements: elements) =>
    switch ArraySlice.get(elements, 0) {
    | Some((Fold_Frac({num, den, superscript}), _)) =>
      let elements = ArraySlice.sliceToEnd(elements, 1)
      let fraction: node = Div(num, den)
      Some((elements, fraction, superscript))
    | _ => None
    }

  let rec parseUntilCloseBracket = (
    ~openBracketRange as (_, openBracketRangeEnd),
    elements: elements,
    startIndex,
  ) => {
    noinline(parseUntilCloseBracket)

    let rec iter = (~bracketLevel, i) => {
      switch ArraySlice.get(elements, i) {
      | Some((Fold_CloseBracket(superscript), (r, _))) if bracketLevel == 1 =>
        let inner = ArraySlice.slice(elements, ~offset=startIndex, ~len=i - startIndex)
        switch parse(~angleRequirement=None, ~rangeEnd=r, inner) {
        | Ok(value) => Ok(i + 1, value, superscript)
        | Error(_) as e => e
        }
      | Some((Fold_OpenBracket, _)) => iter(~bracketLevel=bracketLevel + 1, i + 1)
      | Some((Fold_CloseBracket(_), _)) => iter(~bracketLevel=bracketLevel - 1, i + 1)
      | Some(_) => iter(~bracketLevel, i + 1)
      | None => Error(openBracketRangeEnd)
      }
    }

    iter(~bracketLevel=1, startIndex)
  }
  and parseBracketOrRemaining = (
    ~functionRange as (_, functionRangeEnd),
    elements: elements,
    startIndex,
  ) => {
    noinline(parseBracketOrRemaining)

    switch ArraySlice.get(elements, startIndex) {
    | Some((Fold_OpenBracket, range)) =>
      parseUntilCloseBracket(~openBracketRange=range, elements, startIndex + 1)
    | Some(_, (r, _)) =>
      let remaining = ArraySlice.sliceToEnd(elements, startIndex)
      switch parse(~angleRequirement=None, ~rangeEnd=r, remaining) {
      | Ok(value) => Ok((ArraySlice.length(elements), value, None))
      | Error(_) as e => e
      }
    | None => Error(functionRangeEnd)
    }
  }
  and parseRemaining = (~angleRequirement, ~rangeEnd, elements: elements, initialValue) => {
    noinline(parseRemaining)

    @inline
    let mul = (a, b) =>
      switch a {
      | Some(a) => Some((Mul(a, b): node))
      | None => Some(b)
      }

    let rec iter = (accum, i) =>
      switch ArraySlice.get(elements, i) {
      | None =>
        switch angleRequirement == None ? accum : None {
        | Some(value) => Ok(value)
        | None =>
          switch ArraySlice.get(elements, ArraySlice.length(elements) - 1) {
          | Some((_, (r, _))) => Error(r)
          | None => Error(rangeEnd)
          }
        }
      | Some((Fold_Angle(angle), (_, r'))) =>
        switch angleIsSmaller(~angleRequirement, angle) ? accum : None {
        | Some(accum) =>
          let (i, value) = applyPostFixes(elements, i + 1, applyAngle(accum, angle))
          let after = ArraySlice.sliceToEnd(elements, i)
          switch ArraySlice.get(after, ArraySlice.length(after) - 1) {
          | None => Ok(value)
          | Some((_, (r, _))) =>
            switch parse(~angleRequirement=Some(angle), ~rangeEnd=r, after) {
            | Ok(after) => Ok((Add(value, after): node))
            | Error(_) as e => e
            }
          }
        | None => Error(r')
        }
      | Some((Fold_OpenBracket, openBracketRange)) =>
        switch parseUntilCloseBracket(~openBracketRange, elements, i + 1) {
        | Ok(i, value, superscript) =>
          let value = withSuperscript(value, superscript)
          let (i, value) = applyPostFixes(elements, i, value)
          iter(mul(accum, value), i)
        | Error(_) as e => e
        }
      | Some((Fold_Function({fn, resultSuperscript}), (_, r') as functionRange)) =>
        switch parseBracketOrRemaining(~functionRange, elements, i + 1) {
        | Ok((i, value, superscript)) if resultSuperscript == None || superscript == None =>
          let value =
            applyFunction(fn, value)
            ->withSuperscript(superscript)
            ->withSuperscript(resultSuperscript)
          let (i, value) = applyPostFixes(elements, i, value)
          iter(mul(accum, value), i)
        | Ok(_) => Error(r')
        | Error(_) as e => e
        }
      | Some((element, (_, r'))) =>
        switch handleElement(element) {
        | Some(value) =>
          let (i, value) = applyPostFixes(elements, i + 1, value)
          iter(mul(accum, value), i)
        | None => Error(r')
        }
      }

    iter(initialValue, 0)
  }
  and parseLeadingNumerics = (~angleRequirement, ~rangeEnd, elements) => {
    noinline(parseLeadingNumerics)

    let next = parseRemaining

    switch readNumber(elements) {
    | Ok((elements, current)) =>
      let continue = true

      // Apply postfixes to leading number
      let (elements, current, continue) = switch continue ? current : None {
      | Some(value) =>
        let (i, value) = applyPostFixes(elements, 0, value)
        let elements = ArraySlice.sliceToEnd(elements, i)
        (elements, Some(value), true)
      | None => (elements, current, continue)
      }

      // Handle mixed/lone fractions
      let (elements, current, continue) = switch (
        current,
        continue ? readFraction(elements) : None,
      ) {
      | (Some(value), Some((elements, fraction, superscript))) =>
        // Mixed fraction - don't continue
        let value: node = Add(value, fraction)->withSuperscript(superscript)
        (elements, Some(value), false)
      | (None, Some((elements, fraction, superscript))) =>
        // Lone fraction - continue
        let value = withSuperscript(fraction, superscript)
        (elements, Some(value), true)
      | (_, None) => (elements, current, continue)
      }

      switch !continue ? ArraySlice.get(elements, 0) : None {
      | Some((_, (r, _))) => Error(r)
      | None => next(~angleRequirement, ~rangeEnd, elements, current)
      }
    | Error(_) as e => e
    }
  }
  and parseUnary = (~angleRequirement, ~rangeEnd, elements: elements) => {
    noinline(parseUnary)

    let next = parseLeadingNumerics

    switch ArraySlice.get(elements, 0) {
    | Some(((Fold_Add | Fold_Sub) as op, _)) =>
      let rest = ArraySlice.sliceToEnd(elements, 1)
      switch parseUnary(~angleRequirement, ~rangeEnd, rest) {
      | Ok(root) =>
        let root = op == Fold_Sub ? Node.Neg(root) : root
        Ok(root)
      | Error(_) as e => e
      }
    | _ => next(~angleRequirement, ~rangeEnd, elements)
    }
  }
  // It's hard to unify parsing mul/div and add/sub since one has prefixes
  // and the other postfixes, and it would be more effort to make a generic
  // implementation that handled both
  and parseMulDiv = (~angleRequirement, ~rangeEnd, elements: elements) => {
    noinline(parseMulDiv)

    let iterU = (. angleRequirement, rangeEnd, elements) =>
      parseMulDiv(~angleRequirement, ~rangeEnd, elements)
    let nextU = (. angleRequirement, rangeEnd, elements) =>
      parseUnary(~angleRequirement, ~rangeEnd, elements)

    let rec iter = (~bracketLevel, ~inPostFixPosition, i) =>
      switch ArraySlice.get(elements, i) {
      | Some((Fold_OpenBracket, _)) =>
        iter(~bracketLevel=bracketLevel + 1, ~inPostFixPosition, i - 1)
      | Some((Fold_CloseBracket(_), _)) =>
        iter(~bracketLevel=bracketLevel - 1, ~inPostFixPosition, i - 1)
      | Some(((Fold_Mul | Fold_Div | Fold_Dot) as op, range)) if bracketLevel == 0 =>
        // These operators are never post fixes
        applyBinaryOperator(~angleRequirement, ~rangeEnd, ~iterU, ~nextU, elements, i, op, range)
      | Some((Fold_Percent as op, range)) if bracketLevel == 0 && !inPostFixPosition =>
        applyBinaryOperator(~angleRequirement, ~rangeEnd, ~iterU, ~nextU, elements, i, op, range)
      | Some(_) => iter(~bracketLevel, ~inPostFixPosition=false, i - 1)
      | None => nextU(. angleRequirement, rangeEnd, elements)
      }

    iter(~bracketLevel=0, ~inPostFixPosition=true, ArraySlice.length(elements) - 1)
  }
  and parseAddSub = (~angleRequirement, ~rangeEnd, elements: elements) => {
    noinline(parseAddSub)

    let iterU = (. angleRequirement, rangeEnd, elements) =>
      parseAddSub(~angleRequirement, ~rangeEnd, elements)
    let nextU = (. angleRequirement, rangeEnd, elements) =>
      parseMulDiv(~angleRequirement, ~rangeEnd, elements)

    let rec iter = (~bracketLevel, ~inPrefixPosition, accum, i) =>
      switch ArraySlice.get(elements, i) {
      | Some((Fold_OpenBracket, _)) =>
        iter(~bracketLevel=bracketLevel + 1, ~inPrefixPosition, accum, i + 1)
      | Some((Fold_CloseBracket(_), _)) =>
        iter(~bracketLevel=bracketLevel - 1, ~inPrefixPosition, accum, i + 1)
      | Some(((Fold_Add | Fold_Sub) as op, range)) if bracketLevel == 0 =>
        // Both + and - can be prefixes
        let nextAccum = !inPrefixPosition ? Some((i, op, range)) : accum
        iter(~bracketLevel, ~inPrefixPosition=true, nextAccum, i + 1)
      | Some(_) => iter(~bracketLevel, ~inPrefixPosition=false, accum, i + 1)
      | None =>
        switch accum {
        | Some((i, op, range)) =>
          applyBinaryOperator(~angleRequirement, ~rangeEnd, ~iterU, ~nextU, elements, i, op, range)
        | None => nextU(. angleRequirement, rangeEnd, elements)
        }
      }

    iter(~bracketLevel=0, ~inPrefixPosition=true, None, 0)
  }
  and parse = (~angleRequirement, ~rangeEnd, elements) =>
    parseAddSub(~angleRequirement, ~rangeEnd, elements)

  let parse = elements => {
    let rangeEnd = switch Belt.Array.get(elements, Belt.Array.length(elements) - 1) {
    | Some((_, (_, r'))) => r'
    | None => 0
    }
    ArraySlice.ofArray(elements)->parse(~angleRequirement=None, ~rangeEnd)
  }
  parse
}
