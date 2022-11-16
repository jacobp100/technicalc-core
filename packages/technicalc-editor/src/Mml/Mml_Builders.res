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

let element = (~avoidsSelection=false, ~attributes=list{}, ~superscript=?, ~range=?, tag, body) => {
  let idAttribute: option<Mml_Attributes.t> = switch range {
  | Some((i, i')) =>
    let id = (avoidsSelection ? "~" : "") ++ Belt.Int.toString(i) ++ ":" ++ Belt.Int.toString(i')
    Some((#id, id))
  | None => None
  }

  switch (idAttribute, superscript) {
  | (Some(idAttribute), None) => mml(~attributes=list{idAttribute, ...attributes}, tag, body)
  | (Some(idAttribute), Some({AST.superscriptBody: superscriptBody, index: s})) =>
    let base = mml(~attributes=list{(#id, ":" ++ Belt.Int.toString(s)), ...attributes}, tag, body)
    mml(~attributes=list{idAttribute}, "msup", base ++ superscriptBody)
  | (None, None) => mml(~attributes, tag, body)
  | (None, Some({AST.superscriptBody: superscriptBody})) =>
    let base = mml(~attributes, tag, body)
    mml(~attributes=list{}, "msup", base ++ superscriptBody)
  }
}
