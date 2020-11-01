open EditState_Types;
open EditState_Base;

type countDirection =
  | Forwards
  | Backwards;

let%private skipFunction = (x, ~from, ~direction) => {
  let step = direction == Forwards ? 1 : (-1);
  let rec iter = (~index, ~argLevel) =>
    switch (Belt.Array.get(x, index)) {
    | None => None
    | Some(v) =>
      let index = index + step;
      let argLevel =
        switch (v) {
        | AST.Arg => argLevel - 1
        | _ => argLevel + AST.argCountExn(v)
        };
      switch (compare(argLevel, 0), direction) {
      | (0, _) =>
        let fn = direction == Forwards ? Belt.Array.getExn(x, from) : v;
        Some((index, fn));
      | ((-1), Backwards)
      | (1, Forwards) => iter(~index, ~argLevel)
      | _ => None
      };
    };
  iter(~index=from, ~argLevel=0);
};

let%private skipInsertables = (x: array(AST.t), ~from, ~direction) => {
  let rec iter = (~index, ~bracketLevel) =>
    switch (Belt.Array.get(x, index)) {
    | None
    | Some(Add | Sub | Mul | Div | Dot)
    | Some(Asin | Asinh | Acos | Acosh | Atan | Atanh | Log | Re | Im | SinS)
    | Some(SinhS | CosS | CoshS | TanS | TanhS | Gamma) when bracketLevel == 0 =>
      Some(index)
    | None => None
    | Some(v) =>
      let bracketLevel =
        switch (v) {
        | OpenBracket => bracketLevel + 1
        | CloseBracketS => bracketLevel - 1
        | _ => bracketLevel
        };
      let shouldBreak =
        switch (direction) {
        | Forwards => bracketLevel < 0
        | Backwards => bracketLevel > 0
        };
      let nextIndex =
        shouldBreak ? None : skipFunction(x, ~from=index, ~direction);
      switch (nextIndex) {
      | Some((_, Matrix4S | Matrix9S | Vector2S | Vector3S | Sum2 | Product2))
      | None => Some(index)
      | Some((index, _)) => iter(~index, ~bracketLevel)
      };
    };
  iter(~index=from, ~bracketLevel=0);
};

let%private countInsertables = (x: array(AST.t), ~from, ~direction) =>
  switch (skipInsertables(x, ~from, ~direction)) {
  | Some(index) => abs(from - index)
  | None => 0
  };

let%private insertElement = (elements, element, index) => {
  switch (element) {
  | AST.Superscript1
  | Sqrt1S =>
    let e = countInsertables(elements, ~from=index, ~direction=Forwards);
    let (elements, arg) = ArrayUtil.splice(elements, ~offset=index, ~len=e);
    let combined = Belt.Array.concatMany([|[|element|], arg, [|Arg|]|]);
    let elements = ArrayUtil.insertArray(elements, combined, index);
    (elements, index + 1);
  | NRoot2S =>
    let e = countInsertables(elements, ~from=index, ~direction=Forwards);
    let (elements, radicand) =
      ArrayUtil.splice(elements, ~offset=index, ~len=e);
    let combined =
      Belt.Array.concatMany([|[|element, Arg|], radicand, [|Arg|]|]);
    let elements = ArrayUtil.insertArray(elements, combined, index);
    (elements, index + 1);
  | Frac2S =>
    let s = countInsertables(elements, ~from=index - 1, ~direction=Backwards);
    let e = countInsertables(elements, ~from=index, ~direction=Forwards);
    let (elements, den) = ArrayUtil.splice(elements, ~offset=index, ~len=e);
    let (elements, num) =
      ArrayUtil.splice(elements, ~offset=index - s, ~len=s);
    let frac =
      Belt.Array.concatMany([|[|element|], num, [|Arg|], den, [|Arg|]|]);
    let elements = ArrayUtil.insertArray(elements, frac, index - s);
    let nextIndex = s > 0 ? index + 2 : index + 1;
    (elements, nextIndex);
  | _ =>
    let args =
      switch (AST.argCountExn(element)) {
      | 0 => [|element|]
      | argCount =>
        let args = Belt.Array.make(argCount + 1, AST.Arg);
        Belt.Array.setExn(args, 0, element);
        args;
      };
    let elements = ArrayUtil.insertArray(elements, args, index);
    (elements, index + 1);
  };
};

let%private deleteLabelAtIndex = (elements: array(AST.t), index: int) =>
  switch (Belt.Array.get(elements, index)) {
  | Some(LabelS(_)) =>
    let (elements, _) = ArrayUtil.splice(elements, ~offset=index, ~len=1);
    elements;
  | _ => elements
  };

let insert =
    ({index, elements, allowLabelEditing} as editState, element: AST.t) => {
  let elements = AST.normalize(elements);
  let elements =
    allowLabelEditing ? elements : deleteLabelAtIndex(elements, index);

  if (AST_NormalizationContext.elementIsValid(elements, element, index)) {
    let (elements, index) = insertElement(elements, element, index);
    make(~index, ~elements, ~allowLabelEditing);
  } else {
    editState;
  };
};

let insertArray =
    (
      {index, elements, allowLabelEditing} as editState,
      insertedElements: array(AST.t),
    ) => {
  let elements = AST.normalize(elements);
  let elements =
    allowLabelEditing ? elements : deleteLabelAtIndex(elements, index);

  let valid =
    Belt.Array.every(insertedElements, element =>
      AST_NormalizationContext.elementIsValid(elements, element, index)
    );
  if (valid) {
    let elements = ArrayUtil.insertArray(elements, insertedElements, index);

    let advanceBy =
      Belt.Array.getIndexByU(insertedElements, (. element) =>
        switch (element) {
        | LabelS(_) => true
        | _ => false
        }
      )
      ->Belt.Option.getWithDefault(Belt.Array.length(insertedElements));

    let index = index + advanceBy;
    make(~index, ~elements, ~allowLabelEditing);
  } else {
    editState;
  };
};
