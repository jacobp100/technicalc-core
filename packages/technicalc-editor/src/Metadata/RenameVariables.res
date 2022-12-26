open AST_Types

let renameVariables = (ast: array<t>, mapping) => {
  let didChange = ref(false)

  let mappedAst = Belt.Array.mapU(ast, (. element) =>
    switch element {
    | VariableS({id, symbol}) =>
      switch Belt.Array.getByU(mapping, (. (mappingId, _)) => mappingId == id) {
      | Some((_, mappingSymbol)) if !Symbol.eq(symbol, mappingSymbol) =>
        didChange.contents = true
        VariableS({id, symbol: mappingSymbol})
      | _ => element
      }
    | _ => element
    }
  )

  // Keep referential equality to help React
  didChange.contents ? Some(mappedAst) : None
}
