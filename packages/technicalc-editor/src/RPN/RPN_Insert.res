open RPN_Types

type stackOperation =
  Superscript | NLog | Factorial | Operator(AST.op) | Function(AST.fn) | Element(AST.t)

%%private(
  let stackOperation = (element: AST.t) =>
    switch element {
    | Arg
    | Differential2
    | Product2
    | Sum2
    | Integral3
    | Vector2S
    | Vector3S
    | Matrix4S
    | Matrix9S =>
      None
    | Superscript1 => Some(Superscript)
    | NLog1 => Some(NLog)
    | Factorial => Some(Factorial)
    | _ =>
      switch AST.elementToOp(element) {
      | Some(op) => Some(Operator(op))
      | None =>
        switch AST.elementToFn(element) {
        | Some(fn) => Some(Function(fn))
        | None => AST.argCountExn(element) != 0 ? Some(Element(element)) : None
        }
      }
    }
)

%%private(
  let addBrackets = (elements: array<AST.t>): array<AST.t> =>
    Belt.Array.concatMany([[AST_Types.OpenBracket], elements, [CloseBracketS]])
)

%%private(
  let handleOperation = (operation, stackRev) =>
    switch (stackRev, operation) {
    | (list{superscript, base, ...stackRev}, Superscript) =>
      let elements = Belt.Array.concatMany([addBrackets(base), [Superscript1], superscript, [Arg]])
      Some(list{elements, ...stackRev})
    | (list{arg, base, ...stackRev}, NLog) =>
      let elements = Belt.Array.concatMany([[AST.NLog1], base, [Arg], addBrackets(arg)])
      Some(list{elements, ...stackRev})
    | (list{arg, ...stackRev}, Factorial) =>
      let elements = Belt.Array.concat(addBrackets(arg), [Factorial])
      Some(list{elements, ...stackRev})
    | (list{rhs, lhs, ...stackRev}, Operator(op)) =>
      let elements = Belt.Array.concatMany([
        addBrackets(lhs),
        [AST.opToElement(op)],
        addBrackets(rhs),
      ])
      Some(list{elements, ...stackRev})
    | (list{arg, ...stackRev}, Function(fn)) =>
      let elements = Belt.Array.concatMany([[AST.fnToElement(fn)], addBrackets(arg)])
      Some(list{elements, ...stackRev})
    | (stackRev, Element(element)) =>
      let rec iter = (~elements: array<AST.t>, ~stackRev, ~remaining) =>
        if remaining <= 0 {
          let elements = Belt.Array.concat([element], elements)
          Some(list{elements, ...stackRev})
        } else {
          switch stackRev {
          | list{} => None
          | list{arg, ...stackRev} =>
            iter(
              ~elements=Belt.Array.concatMany([arg, [Arg], elements]),
              ~stackRev,
              ~remaining=remaining - 1,
            )
          }
        }
      iter(~elements=[], ~stackRev, ~remaining=AST.argCountExn(element))
    | _ => None
    }
)

%%private(
  let hasPendingOpenBracket = (elements: array<AST.t>) =>
    EditState_ASTUtil.bracketLevel(elements, ~from=0, ~direction=Forwards) > 0
)

let insert = (rpn: t, editState: EditState.t, element: AST.t) => {
  let isUnaryMinus = element == Sub && EditState.isEmpty(editState)
  let canHandleStackOperation =
    !isUnaryMinus &&
    editState.index == Belt.Array.length(editState.elements) &&
    !hasPendingOpenBracket(editState.elements)
  let stackOperation = canHandleStackOperation ? stackOperation(element) : None

  switch stackOperation {
  | Some(operation) =>
    switch RPN_Submit.submit(~pushLashWhenEmpty=false, rpn, editState) {
    | Ok(rpn) =>
      switch handleOperation(operation, rpn.stackRev) {
      | Some(stackRev) => Ok(({stackRev: stackRev}, EditState.empty))
      | None => Error(None)
      }
    | Error(i) => Error(Some(i))
    }
  | None => Ok((rpn, EditState.insert(editState, element)))
  }
}

%%private(
  let gatherFunctionArguments = (elements: array<AST.t>, index) => {
    let rec iter = (~argsRev, ~functionStartIndex, index) =>
      switch Belt.Array.get(elements, index) {
      | Some(Arg) =>
        let functionEndIndex = index
        let args = Belt.Array.slice(
          elements,
          ~offset=functionStartIndex,
          ~len=functionEndIndex - functionStartIndex,
        )
        iter(~argsRev=list{args, ...argsRev}, ~functionStartIndex=index + 1, index + 1)
      | Some(_) =>
        switch EditState_ASTUtil.advanceIndex(elements, index, ~direction=Forwards) {
        | Some((index, _)) => iter(~argsRev, ~functionStartIndex, index)
        | None => argsRev
        }
      | None => argsRev
      }

    iter(~argsRev=list{}, ~functionStartIndex=1, index + 1)
  }
)

let insertArray = (rpn: t, editState: EditState.t, elements: array<AST.t>) => {
  let rpnFn = switch RPN_Submit.submit(~pushLashWhenEmpty=false, rpn, editState) {
  | Ok(rpn) =>
    switch EditState_ASTUtil.advanceIndex(elements, 0, ~direction=Forwards) {
    | Some((initialFunctionEndIndex, element))
      if initialFunctionEndIndex === Belt.Array.length(elements) =>
      Some((rpn, element))
    | _ => None
    }
  | Error(_) => None
  }

  let rpnFnArgs = switch rpnFn {
  | Some((rpn, element)) =>
    let argsRev = gatherFunctionArguments(elements, 1)
    if AST.argCountExn(element) == Belt.List.length(argsRev) {
      Some((rpn, element, argsRev))
    } else {
      None
    }
  | None => None
  }

  switch rpnFnArgs {
  | Some((rpn, element, argsRev)) =>
    let isFunction = element == NLog1

    let rec iterRev = (
      ~elements: array<AST.t>,
      ~stackRev: list<array<AST.t>>,
      argsRev: list<array<AST.t>>,
    ) => {
      switch argsRev {
      | list{[], ...argsRev} =>
        switch stackRev {
        | list{arg, ...stackRev} =>
          let elements = Belt.Array.concatMany([arg, [Arg], elements])
          iterRev(~elements, ~stackRev, argsRev)
        | list{} => Error(None)
        }
      | list{arg, ...argsRev} =>
        let elements = Belt.Array.concatMany([arg, [Arg], elements])
        iterRev(~elements, ~stackRev, argsRev)
      | list{} =>
        let elementsStackRev = if isFunction {
          switch stackRev {
          | list{arg, ...stackRev} => Some((Belt.Array.concat(elements, arg), stackRev))
          | list{} => None
          }
        } else {
          Some((elements, stackRev))
        }

        switch elementsStackRev {
        | Some((elements, stackRev)) =>
          let elements = Belt.Array.concat([element], elements)
          let stackRev = list{elements, ...stackRev}
          let rpn = {stackRev: stackRev}
          Ok((rpn, EditState.empty))
        | None => Error(None)
        }
      }
    }
    iterRev(~elements=[], ~stackRev=rpn.stackRev, argsRev)
  | None => Ok((rpn, EditState.insertArray(editState, elements)))
  }
}
