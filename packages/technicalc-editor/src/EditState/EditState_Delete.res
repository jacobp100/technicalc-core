open EditState_Types
open EditState_Base

%%private(
  let rec matchNEmptyArgs = (elements, ~index, ~count) =>
    if count == 0 {
      true
    } else {
      switch Belt.Array.get(elements, index) {
      | Some(AST.Arg) => matchNEmptyArgs(elements, ~index=index + 1, ~count=count - 1)
      | _ => false
      }
    }
)

%%private(
  let nArgsSlice = (~skipInitial=0, elements, index) => {
    let element = Belt.Array.getExn(elements, index)
    let count = AST.argCountExn(element)
    let current = ref([])

    let next = ref(index + 1)
    for i in 0 to count - 1 {
      let offset = next.contents
      next := AST.argEndIndex(elements, offset)
      let len = next.contents - offset - 1
      if i >= skipInitial {
        let slice = Belt.Array.slice(elements, ~offset, ~len)
        current := Belt.Array.concat(current.contents, slice)
      }
    }

    current.contents
  }
)

type deletionMode =
  | Keep
  | Delete(int)
  | Spread(array<AST.t>)

%%private(
  let isCloseBracketAtIndex = (elements, index) =>
    switch Belt.Array.get(elements, index) {
    | Some(AST.CloseBracketS) => true
    | _ => false
    }
)

%%private(
  let deletionMode = (elements, index) =>
    switch Belt.Array.get(elements, index) {
    | Some(AST.Arg | CaptureGroupStart(_) | CaptureGroupEndS) => Keep
    | Some(Superscript1 | Sqrt1S | Frac2S) => Spread(nArgsSlice(elements, index))
    | Some(NRoot2S) =>
      let degreeIsEmpty = matchNEmptyArgs(elements, ~index=index + 1, ~count=1)
      degreeIsEmpty ? Spread(nArgsSlice(elements, index, ~skipInitial=1)) : Keep
    | Some(OpenBracket) if isCloseBracketAtIndex(elements, index + 1) => Delete(2)
    | Some(v) =>
      let argCount = AST.argCountExn(v)
      let argsEmpty = matchNEmptyArgs(elements, ~index=index + 1, ~count=argCount)
      argsEmpty ? Delete(1) : Keep
    | None => Keep
    }
)

%%private(
  let deleteAtIndexExn = (elements, startIndex) => {
    let element = Belt.Array.getExn(elements, startIndex)
    let argCount = AST.argCountExn(element)
    let endIndex = ref(startIndex + 1)
    for _ in 1 to argCount {
      endIndex := AST.argEndIndex(elements, endIndex.contents)
    }
    let endIndex = endIndex.contents
    if endIndex >= Belt.Array.length(elements) {
      Belt.Array.slice(elements, ~offset=0, ~len=startIndex)
    } else {
      Belt.Array.concat(
        Belt.Array.slice(elements, ~offset=0, ~len=startIndex),
        Belt.Array.sliceToEnd(elements, endIndex),
      )
    }
  }
)

%%private(
  @inline
  let deleteNAtIndexExn = (elements, index, ~count) => {
    let elements = ref(elements)
    for _ in 1 to count {
      elements := deleteAtIndexExn(elements.contents, index)
    }
    elements.contents
  }
)

%%private(
  let deleteEmptySuperscript = (elements, index) =>
    switch (Belt.Array.get(elements, index), Belt.Array.get(elements, index + 1)) {
    | (Some(AST.Superscript1), Some(Arg)) =>
      Belt.Array.concat(
        Belt.Array.slice(elements, ~offset=0, ~len=index),
        Belt.Array.sliceToEnd(elements, index + 2),
      )
    | _ => elements
    }
)

let delete = (editState: t) => {
  let {index, elements, formatCaptureGroups} = editState
  let elements = AST.normalize(elements)

  let possibleEmptyCaptureGroupStart = formatCaptureGroups ? index - 2 : index - 1

  let isEmptyCaptureGroup = EditState_Util.isEmptyCaptureGroup(
    elements,
    possibleEmptyCaptureGroupStart,
  )

  if isEmptyCaptureGroup {
    let index = possibleEmptyCaptureGroupStart
    let (elements, _) = ArrayUtil.splice(elements, ~offset=index, ~len=2)
    make(~index, ~elements, ~formatCaptureGroups)
  } else if index > 0 {
    let index = index - 1
    let elements = switch deletionMode(elements, index) {
    | Keep => elements
    | Delete(count) => deleteNAtIndexExn(elements, index, ~count)->deleteEmptySuperscript(index)
    | Spread(spread) =>
      deleteAtIndexExn(elements, index)
      ->deleteEmptySuperscript(index)
      ->ArrayUtil.insertArray(spread, index)
    }
    make(~index, ~elements, ~formatCaptureGroups)
  } else {
    editState
  }
}
