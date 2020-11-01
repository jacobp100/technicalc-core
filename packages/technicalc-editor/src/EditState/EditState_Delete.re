open EditState_Types;
open EditState_Base;

let%private isIsolatedLabel = (elements, index) =>
  switch (Belt.Array.get(elements, index)) {
  | Some(AST.LabelS(_)) =>
    switch (Belt.Array.get(elements, index - 1)) {
    | Some(LabelS(_)) => false
    | _ => true
    }
  | _ => false
  };

let%private rec matchNEmptyArgs = (elements, ~index, ~count) =>
  if (count == 0) {
    true;
  } else {
    switch (Belt.Array.get(elements, index)) {
    | Some(AST.Arg) =>
      matchNEmptyArgs(elements, ~index=index + 1, ~count=count - 1)
    | _ => false
    };
  };

let%private nArgsSlice = (~skipInitial=0, elements, index) => {
  let element = Belt.Array.getExn(elements, index);
  let count = AST.argCountExn(element);
  let current = ref([||]);

  let next = ref(index + 1);
  for (i in 0 to count - 1) {
    let offset = next^;
    next := AST.argEndIndex(elements, offset);
    let len = next^ - offset - 1;
    if (i >= skipInitial) {
      let slice = Belt.Array.slice(elements, ~offset, ~len);
      current := Belt.Array.concat(current^, slice);
    };
  };

  current^;
};

type deletionMode =
  | Keep
  | Delete
  | Spread(array(AST.t));

let%private deletionMode = (elements, index) =>
  switch (Belt.Array.get(elements, index)) {
  | Some(AST.Arg) => Keep
  | Some(Superscript1 | Sqrt1S | Frac2S) =>
    Spread(nArgsSlice(elements, index))
  | Some(NRoot2S) =>
    let degreeIsEmpty = matchNEmptyArgs(elements, ~index=index + 1, ~count=1);
    degreeIsEmpty
      ? Spread(nArgsSlice(elements, index, ~skipInitial=1)) : Keep;
  | Some(v) =>
    let argCount = AST.argCountExn(v);
    let argsEmpty =
      matchNEmptyArgs(elements, ~index=index + 1, ~count=argCount);
    argsEmpty ? Delete : Keep;
  | None => Keep
  };

let%private deleteAtIndexExn = (elements, startIndex) => {
  let element = Belt.Array.getExn(elements, startIndex);
  let argCount = AST.argCountExn(element);
  let endIndex = ref(startIndex + 1);
  for (_ in 1 to argCount) {
    endIndex := AST.argEndIndex(elements, endIndex^);
  };
  let endIndex = endIndex^;
  if (endIndex >= Belt.Array.length(elements)) {
    Belt.Array.slice(elements, ~offset=0, ~len=startIndex);
  } else {
    Belt.Array.concat(
      Belt.Array.slice(elements, ~offset=0, ~len=startIndex),
      Belt.Array.sliceToEnd(elements, endIndex),
    );
  };
};

let%private deleteEmptySuperscript = (elements, index) =>
  switch (
    Belt.Array.get(elements, index),
    Belt.Array.get(elements, index + 1),
  ) {
  | (Some(AST.Superscript1), Some(Arg)) =>
    Belt.Array.concat(
      Belt.Array.slice(elements, ~offset=0, ~len=index),
      Belt.Array.sliceToEnd(elements, index + 2),
    )
  | _ => elements
  };

let delete = (editState: t) => {
  let {index, elements, allowLabelEditing} = editState;
  let elements = AST.normalize(elements);

  if (!allowLabelEditing && isIsolatedLabel(elements, index)) {
    let (elements, _) = ArrayUtil.splice(elements, ~offset=index, ~len=1);
    make(~index, ~elements, ~allowLabelEditing);
  } else if (index > 0) {
    let index = index - 1;
    let elements =
      switch (deletionMode(elements, index)) {
      | Keep => elements
      | Delete =>
        deleteAtIndexExn(elements, index)->deleteEmptySuperscript(index)
      | Spread(spread) =>
        deleteAtIndexExn(elements, index)
        ->deleteEmptySuperscript(index)
        ->ArrayUtil.insertArray(spread, index)
      };
    make(~index, ~elements, ~allowLabelEditing);
  } else {
    editState;
  };
};
