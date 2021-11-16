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
  let gatherFunctionArguments = (elements: array<AST.t>, ~count) => {
    let rec iter = (~argsRev, ~remaining, ~functionStartIndex, index) =>
      switch Belt.Array.get(elements, index) {
      | Some(Arg) =>
        let functionEndIndex = index
        let args = Belt.Array.slice(
          elements,
          ~offset=functionStartIndex,
          ~len=functionEndIndex - functionStartIndex,
        )
        iter(
          ~argsRev=list{args, ...argsRev},
          ~remaining=remaining - 1,
          ~functionStartIndex=index + 1,
          index + 1,
        )
      | Some(_) =>
        switch AST.advanceIndex(elements, index, ~direction=Forwards) {
        | Some((index, _)) => iter(~argsRev, ~remaining, ~functionStartIndex, index)
        | None => None
        }
      | None => remaining == 0 ? Some(argsRev) : None
      }

    iter(~argsRev=list{}, ~remaining=count, ~functionStartIndex=1, 1)
  }
)

%%private(
  let handleArgs = (stackRev: list<array<AST.t>>, elements: array<AST.t>) => {
    let element = Belt.Array.getExn(elements, 0)
    let count = AST.argCountExn(element)

    switch gatherFunctionArguments(elements, ~count) {
    | Some(argsRev) =>
      let rec iterRev = (
        ~elementsOut: array<AST.t>,
        ~stackRev: list<array<AST.t>>,
        ~argsRev: list<array<AST.t>>,
      ) => {
        switch argsRev {
        | list{[], ...argsRev} =>
          switch stackRev {
          | list{arg, ...stackRev} =>
            let elementsOut = Belt.Array.concatMany([arg, [Arg], elementsOut])
            iterRev(~elementsOut, ~stackRev, ~argsRev)
          | list{} => None
          }
        | list{arg, ...argsRev} =>
          let elementsOut = Belt.Array.concatMany([arg, [Arg], elementsOut])
          iterRev(~elementsOut, ~stackRev, ~argsRev)
        | list{} =>
          let elements = Belt.Array.concat([element], elementsOut)
          let stackRev = list{elements, ...stackRev}
          Some(stackRev)
        }
      }

      iterRev(~elementsOut=[], ~stackRev, ~argsRev)
    | None => None
    }
  }
)

%%private(
  let addBrackets = (elements: array<AST.t>): array<AST.t> =>
    Belt.Array.concatMany([[AST_Types.OpenBracket], elements, [CloseBracketS]])
)

%%private(
  let operatorPrecedence = (op: AST.op) =>
    switch op {
    | Op_Add | Op_Sub => 1
    | Op_Mul | Op_Div | Op_Dot | Op_Rem => 0
    }
)

%%private(
  let highestOperatorPrecedence = (elements: array<AST.t>) => {
    let rec iter = (~maxPrecedence, ~bracketLevel, index) => {
      let element = Belt.Array.get(elements, index)
      let bracketLevel = switch element {
      | Some(OpenBracket) => bracketLevel + 1
      | Some(CloseBracketS) => bracketLevel - 1
      | _ => bracketLevel
      }
      let op = switch element {
      | Some(element) if bracketLevel == 0 => AST.elementToOp(element)
      | _ => None
      }
      let maxPrecedence = switch (maxPrecedence, op) {
      | (None, Some(op)) => Some(operatorPrecedence(op))
      | (Some(maxPrecedence), Some(op)) => max(operatorPrecedence(op), maxPrecedence)->Some
      | (maxPrecedence, None) => maxPrecedence
      }
      switch AST.advanceIndex(elements, index) {
      | Some((index, _)) => iter(~maxPrecedence, ~bracketLevel, index)
      | None => maxPrecedence
      }
    }
    iter(~maxPrecedence=None, ~bracketLevel=0, 0)
  }
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
      let precedence = operatorPrecedence(op)
      let rhs = switch highestOperatorPrecedence(rhs) {
      | Some(rhsPrecedence) if rhsPrecedence > precedence => addBrackets(rhs)
      | _ => rhs
      }
      let lhs = switch highestOperatorPrecedence(lhs) {
      | Some(lhsPrecedence) if lhsPrecedence > precedence => addBrackets(lhs)
      | _ => lhs
      }

      let elements = Belt.Array.concatMany([lhs, [AST.opToElement(op)], rhs])
      Some(list{elements, ...stackRev})
    | (list{arg, ...stackRev}, Function(fn)) =>
      let elements = Belt.Array.concatMany([[AST.fnToElement(fn)], addBrackets(arg)])
      Some(list{elements, ...stackRev})
    | (stackRev, Element(element)) =>
      let elements = Belt.Array.concat(
        [element],
        Belt.Array.make(AST.argCountExn(element), AST.Arg),
      )
      handleArgs(stackRev, elements)
    | _ => None
    }
)

%%private(
  let hasPendingOpenBracket = (elements: array<AST.t>) =>
    AST.bracketLevel(elements, ~from=0, ~direction=Forwards) > 0
)

let insert = (rpn: t, editState: EditState.t, element: AST.t) => {
  // TODO: Unary minus
  let canHandleStackOperation =
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

let insertArray = (rpn: t, editState: EditState.t, elements: array<AST.t>) => {
  switch RPN_Submit.submit(~pushLashWhenEmpty=false, rpn, editState) {
  | Ok(rpn) =>
    switch AST.advanceIndex(elements, 0, ~direction=Forwards) {
    | Some((initialFunctionEndIndex, fn))
      if initialFunctionEndIndex === Belt.Array.length(elements) =>
      let stackRev = rpn.stackRev

      let isFunction = fn == NLog1 || AST.elementToFn(fn) != None
      let (endElements, stackRev) = if isFunction {
        switch stackRev {
        | list{arg, ...stackRev} => (Some(addBrackets(arg)), Some(stackRev))
        | list{} => (None, None)
        }
      } else {
        (None, Some(stackRev))
      }

      let handleArgsStackRev = switch stackRev {
      | Some(stackRev) => handleArgs(stackRev, elements)
      | None => None
      }

      switch (handleArgsStackRev, endElements) {
      | (Some(list{elements, ...stackRev}), Some(endElements)) =>
        let elements = Belt.Array.concat(elements, endElements)
        let stackRev = list{elements, ...stackRev}
        Ok(({stackRev: stackRev}, EditState.empty))
      | (Some(stackRev), None) => Ok(({stackRev: stackRev}, EditState.empty))
      | _ => Error(None)
      }
    | _ => Ok((rpn, EditState.insertArray(editState, elements)))
    }
  | Error(_) => Ok((rpn, EditState.insertArray(editState, elements)))
  }
}
