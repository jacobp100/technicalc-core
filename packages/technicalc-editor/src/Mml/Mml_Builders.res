%%private(
  let mml = (~attributes, tag, body) => {
    let elementWithAttributes = switch attributes {
    | list{} => tag
    | attributes =>
      let attributes =
        attributes
        ->Belt.List.toArray
        ->Belt.Array.keepMapU((. attribute) => Mml_Attributes.toString(attribute))
        ->StringUtil.joinWith(" ")

      tag ++ " " ++ attributes
    }

    `<${elementWithAttributes}>${body}</${tag}>`
  }
)

%%private(
  let optionNumberToString = x =>
    switch x {
    | Some(x) => Belt.Int.toString(x)
    | None => ""
    }
)

let selection = (~avoid=false, ~start=?, ~end=?, ()): Mml_Attributes.t => (
  #id,
  (avoid ? "~" : "") ++ optionNumberToString(start) ++ ":" ++ optionNumberToString(end),
)

let element = (~avoidsSelection=?, ~attributes=list{}, ~superscript=?, ~range=?, tag, body) => {
  let selectionAttribute: option<Mml_Attributes.t> = switch range {
  | Some((start, end)) => Some(selection(~avoid=?avoidsSelection, ~start, ~end, ()))
  | None => None
  }

  switch (selectionAttribute, superscript) {
  | (Some(selectionAttribute), None) =>
    mml(~attributes=list{selectionAttribute, ...attributes}, tag, body)
  | (Some(selectionAttribute), Some({AST.superscriptBody: superscriptBody, index: s})) =>
    let base = mml(~attributes=list{selection(~end=s, ()), ...attributes}, tag, body)
    mml(~attributes=list{selectionAttribute}, "msup", base ++ superscriptBody)
  | (None, None) => mml(~attributes, tag, body)
  | (None, Some({AST.superscriptBody: superscriptBody})) =>
    let base = mml(~attributes, tag, body)
    mml(~attributes=list{}, "msup", base ++ superscriptBody)
  }
}
