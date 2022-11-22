open Mml_Util

%%private(
  let regExpMatchAtIndex = (matchGroup: option<array<option<string>>>, index) =>
    switch matchGroup {
    | Some(matches) =>
      switch Belt.Array.getExn(matches, index) {
      | Some(match) => match
      | None => ""
      }
    | None => ""
    }
)

// Deprecated - used to migrate
let ofMml = (mml: string): Symbol.t => {
  let variantIndex = 1
  let mmlIndex = 2
  let regExp = %re(`/(?:<mi mathvariant="(\w+)"[^>]*>([^<>]*)<\/mi>)/g`)

  let baseMatch = Js.String.match_(regExp, mml)
  let mathVairant = regExpMatchAtIndex(baseMatch, variantIndex)
  let bold = StringUtil.includes(mathVairant, "bold")
  let italic = StringUtil.includes(mathVairant, "italic")
  let base = regExpMatchAtIndex(baseMatch, mmlIndex)
  let base = base != Placeholder.body ? base : ""

  let (subscript, superscript) = if StringUtil.startsWith(mml, "<msub>") {
    let subscript = Js.String.match_(regExp, mml)->regExpMatchAtIndex(mmlIndex)
    (subscript, "")
  } else if StringUtil.startsWith(mml, "<msup>") {
    let superscript = Js.String.match_(regExp, mml)->regExpMatchAtIndex(mmlIndex)
    ("", superscript)
  } else if StringUtil.startsWith(mml, "<msubsup>") {
    let subscript = Js.String.match_(regExp, mml)->regExpMatchAtIndex(mmlIndex)
    let superscript = Js.String.match_(regExp, mml)->regExpMatchAtIndex(mmlIndex)
    (subscript, superscript)
  } else {
    ("", "")
  }

  {bold, italic, base, subscript, superscript}
}

let toMml = (x: Symbol.t) => {
  let style = switch x {
  | {bold: false, italic: false} => "normal"
  | {bold: false, italic: true} => "italic"
  | {bold: true, italic: false} => "bold"
  | {bold: true, italic: true} => "bold-italic"
  }

  let base =
    x.base != ""
      ? `<mi mathvariant="${style}">${x.base}</mi>`
      : `<mi mathvariant="normal" class="placeholder">${Placeholder.body}</mi>`

  let superscript = `<mi mathvariant="normal">${x.superscript}</mi>`
  let subscript = `<mi mathvariant="normal">${x.subscript}</mi>`

  switch x {
  | {superscript: "", subscript: ""} => base
  | {superscript: ""} => `<msub>${base}${subscript}</msub>`
  | {subscript: ""} => `<msup>${base}${superscript}</msup>`
  | _ => `<msubsup>${base}${subscript}${superscript}</msubsup>`
  }
}
