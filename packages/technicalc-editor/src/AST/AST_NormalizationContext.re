let%private validityStackReducer = prependValidityStack => {
  let reducerFn = ((range, validityStack), element, i) => {
    let range =
      switch (validityStack) {
      | [false, ..._] => Ranges.addSequentialIndex(range, i)
      | _ => range
      };
    let validityStack =
      switch (element) {
      | AST_Types.Arg =>
        Belt.List.tail(validityStack)
        ->Belt.Option.getWithDefault(validityStack)
      | e => prependValidityStack(. validityStack, e)
      };
    (range, validityStack);
  };
  ast => Belt.Array.reduceWithIndex(ast, (Ranges.empty, []), reducerFn)->fst;
};

let%private noTableRanges =
  validityStackReducer((. validityStack, element) =>
    switch (element) {
    | AST_Types.Frac2S => [/* num */ true, /* den */ false, ...validityStack]
    | Abs1S
    | Floor1S
    | Ceil1S
    | Round1S => [true, ...validityStack]
    | _ =>
      let argCount = AST_Types.argCountExn(element);
      validityStack->ListUtil.prependMany(argCount, false);
    }
  );

let%private isIterator = element =>
  switch (element) {
  | AST_Types.Sum2
  | Product2
  | Differential2
  | Integral3 => true
  | _ => false
  };

let%private isTable = element =>
  switch (element) {
  | AST_Types.Vector2S
  | Vector3S
  | Matrix4S
  | Matrix9S => true
  | _ => false
  };

let%private noIterationRanges =
  validityStackReducer((. validityStack, element) => {
    let argCount = AST_Types.argCountExn(element);
    validityStack->ListUtil.prependMany(argCount, !isIterator(element));
  });

let elementIsValid = (ast: array(AST_Types.t), element: AST_Types.t, index) =>
  if (isIterator(element)) {
    !noIterationRanges(ast)->Ranges.contains(index);
  } else if (isTable(element)) {
    !noTableRanges(ast)->Ranges.contains(index);
  } else {
    true;
  };
