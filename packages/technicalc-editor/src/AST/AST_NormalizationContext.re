type elementClass =
  | Table
  | Iterator
  | Other;

let classify = element =>
  switch (element) {
  | AST_Types.Sum2
  | Product2
  | Differential2
  | Integral3 => Iterator
  | Vector2S
  | Vector3S
  | Matrix4S
  | Matrix9S => Table
  | _ => Other
  };

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

let noTableRanges =
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

let noIterationRanges =
  validityStackReducer((. validityStack, element) => {
    let argCount = AST_Types.argCountExn(element);
    validityStack->ListUtil.prependMany(
      argCount,
      classify(element) != Iterator,
    );
  });

let elementIsValid = (ast: array(AST_Types.t), element: AST_Types.t, index) =>
  switch (classify(element)) {
  | Iterator => !noIterationRanges(ast)->Ranges.contains(index)
  | Table => !noTableRanges(ast)->Ranges.contains(index)
  | Other => true
  };
