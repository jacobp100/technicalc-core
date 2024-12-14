open AST

let reifyPlaceholders = (elements: array<t>) => {
  PlaceholderMetadata.placeholders(elements)->Belt.Array.reduceReverse(elements, (
    elements,
    placeholder,
  ) => {
    switch placeholder {
    | Implicit({index}) =>
      let insertedElements = [CaptureGroupStart({placeholder: None}), CaptureGroupEndS]
      let elements = ArrayUtil.insertArray(elements, insertedElements, index)
      elements
    | Explicit(_) => elements
    }
  })
}
