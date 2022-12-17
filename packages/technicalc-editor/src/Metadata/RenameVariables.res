open AST_Types

let renameVariables = (ast: array<t>, mapping) => {
  let didChange = ref(false)

  let mappedAst = Belt.Array.mapU(ast, (. element) =>
    switch element {
    | VariableS({id, name}) =>
      switch Belt.Array.getByU(mapping, (. (mappingId, _)) => mappingId == id) {
      | Some((_, mappingName)) if name != mappingName =>
        didChange.contents = true
        VariableS({id, name: mappingName})
      | _ => element
      }
    | _ => element
    }
  )

  // Keep referential equality to help React
  didChange.contents ? Some(mappedAst) : None
}
