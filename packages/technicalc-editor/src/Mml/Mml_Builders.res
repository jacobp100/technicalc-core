let createElement = (~attributes=list{}, element, body) => {
  let elementWithAttributes = switch attributes {
  | list{} => element
  | attributes =>
    let attributes =
      attributes
      ->Belt.List.mapU((. (p, v)) => `${p}="${v}"`)
      ->Belt.List.toArray
      ->StringUtil.joinWith(" ")

    element ++ " " ++ attributes
  }

  `<${elementWithAttributes}>${body}</${element}>`
}

let createSuperscript = (
  ~containerAttributes=list{},
  ~attributes=list{},
  superscript,
  element,
  body,
) => {
  let {AST.superscriptBody: superscriptBody, index: s} = superscript
  let base = createElement(
    ~attributes=list{("id", ":" ++ Belt.Int.toString(s)), ...attributes},
    element,
    body,
  )
  createElement(~attributes=containerAttributes, "msup", base ++ superscriptBody)
}

let createElementWithRange = (~attributes=list{}, ~superscript=?, (i, i'), element, body) => {
  let idAttribute = ("id", Belt.Int.toString(i) ++ ":" ++ Belt.Int.toString(i'))

  switch superscript {
  | None => createElement(~attributes=list{idAttribute, ...attributes}, element, body)
  | Some(superscript) =>
    createSuperscript(
      ~containerAttributes=list{idAttribute},
      ~attributes,
      superscript,
      element,
      body,
    )
  }
}
