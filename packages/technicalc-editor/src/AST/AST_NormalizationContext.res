type elementClass =
  | Table
  | Iterator
  | Other

let classify = element =>
  switch element {
  | AST_Types.Sum2
  | Product2
  | Differential2
  | Integral3 =>
    Iterator
  | TableNS(_) => Table
  | _ => Other
  }

%%private(
  @inline
  let tailOrEmpty = l =>
    switch l {
    | list{_, ...tail} => tail
    | list{} => list{}
    }
)

%%private(
  @inline
  let addSequentialIndex = (x, i) =>
    switch x {
    | list{(a, b), ...rest} if b == i - 1 => list{(a, i), ...rest}
    | rest => list{(i, i), ...rest}
    }
)
%%private(
  let rec isValid = validityStack =>
    switch validityStack {
    | list{} => true
    | list{false, ..._} => false
    | list{_, ...rest} => isValid(rest)
    }
)

%%private(
  let validityStackReducer = prependValidityStack => {
    let reducerFn = (. (range, validityStack), element, i) => {
      let range = if !isValid(validityStack) {
        addSequentialIndex(range, i)
      } else {
        range
      }
      let validityStack = switch element {
      | AST_Types.Arg => tailOrEmpty(validityStack)
      | e => prependValidityStack(. validityStack, e)
      }
      (range, validityStack)
    }
    ast => Belt.Array.reduceWithIndexU(ast, (Ranges.empty, list{}), reducerFn)->fst
  }
)

let noTablePermittedRanges = validityStackReducer((. validityStack, element) =>
  switch element {
  | AST_Types.Frac2S => list{/* num */ true, /* den */ false, ...validityStack}
  | Abs1S
  | Floor1S
  | Ceil1S
  | Round1S
  | TableNS(_) =>
    let argCount = AST_Types.argCountExn(element)
    validityStack->ListUtil.prependMany(argCount, true)
  | _ =>
    let argCount = AST_Types.argCountExn(element)
    validityStack->ListUtil.prependMany(argCount, false)
  }
)

let noIterationPermittedRanges = validityStackReducer((. validityStack, element) => {
  let argCount = AST_Types.argCountExn(element)
  validityStack->ListUtil.prependMany(argCount, classify(element) != Iterator)
})

let elementIsValid = (ast: array<AST_Types.t>, element: AST_Types.t, index) =>
  switch classify(element) {
  | Iterator => !(noIterationPermittedRanges(ast)->Ranges.contains(index))
  | Table => !(noTablePermittedRanges(ast)->Ranges.contains(index))
  | Other => true
  }
