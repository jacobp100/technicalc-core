open Mml_Builders

%%private(
  let optionNumberToString = x =>
    switch x {
    | Some(x) => Belt.Int.toString(x)
    | None => ""
    }
)

%%private(
  let selection = (~avoid=false, ~start=?, ~end=?, ()): Mml_Attributes.t => (
    #id,
    (avoid ? "~" : "") ++ optionNumberToString(start) ++ ":" ++ optionNumberToString(end),
  )
)

%%private(
  let element = (
    ~metadata,
    ~avoidsSelection=?,
    ~attributes=list{},
    ~superscript=?,
    ~range=?,
    tag,
    body,
  ) => {
    let selectionAttribute: option<Mml_Attributes.t> = switch range {
    | Some((start, end)) => Some(selection(~avoid=?avoidsSelection, ~start, ~end, ()))
    | None => None
    }

    switch (selectionAttribute, superscript) {
    | (Some(selectionAttribute), None) =>
      mml(~metadata, ~attributes=list{selectionAttribute, ...attributes}, tag, body)
    | (Some(selectionAttribute), Some({AST.superscriptBody: superscriptBody, index: s})) =>
      let base = mml(~metadata, ~attributes=list{selection(~end=s, ()), ...attributes}, tag, body)
      mml(~metadata, ~attributes=list{selectionAttribute}, "msup", base ++ superscriptBody)
    | (None, None) => mml(~metadata, ~attributes, tag, body)
    | (None, Some({AST.superscriptBody: superscriptBody})) =>
      let base = mml(~metadata, ~attributes, tag, body)
      mml(~metadata, ~attributes=list{}, "msup", base ++ superscriptBody)
    }
  }
)

%%private(let invalidAttributes = list{(#class, "invalid"), (#stretchy, "false")})

include Stringifier.Make({
  let groupingSeparatorU = (. groupingSeparator) =>
    groupingSeparator == " "
      ? mml(~attributes=list{(#width, "4px")}, "mspace", "")
      : mml("mn", groupingSeparator)
  let decimalSeparatorU = (. decimalSeparator, range, metadata) =>
    element(~metadata, ~range, "mn", decimalSeparator)

  let unpairedOpenBracketU = (. range, metadata) =>
    element(~metadata, ~attributes=invalidAttributes, ~range, "mo", "(")
  let unpairedCloseBracketU = (. superscript, range, metadata) =>
    element(~metadata, ~attributes=invalidAttributes, ~superscript?, ~range, "mo", ")")

  let bracketRangeU = (. superscript, body, openBracketRange, closeBracketRange, metadata) =>
    switch superscript {
    | Some({AST.superscriptBody: superscriptBody, index: superscriptIndex}) =>
      // We want the superscript to be over the whole bracket group,
      // not just over the close bracket
      // Every other element works differently to this
      let (closeBracketStart, closeBracketEnd) = closeBracketRange
      let body =
        element(~metadata, ~range=openBracketRange, "mo", "(") ++
        body ++
        element(~metadata, ~range=(closeBracketStart, superscriptIndex), "mo", ")")
      element(
        ~metadata,
        ~attributes=list{selection(~end=closeBracketEnd, ())},
        "msup",
        mml("mrow", body) ++ superscriptBody,
      )
    | None =>
      // Always wrap in mrow so bracket heights don't change when adding superscript
      mml(
        "mrow",
        element(~metadata, ~range=openBracketRange, "mo", "(") ++
        body ++
        element(~metadata, ~range=closeBracketRange, "mo", ")"),
      )
    }
})

let append = (v, ~avoidsSelection=?, ~attributes=?, ~superscript=?, ~range=?, tag, body) =>
  element(
    ~metadata=format(. v).metadata,
    ~avoidsSelection?,
    ~attributes?,
    ~superscript?,
    ~range?,
    tag,
    body,
  )->append(. v, _)

let appendOperatorOrFunction = (
  v,
  ~avoidsSelection=?,
  ~attributes=?,
  ~superscript=?,
  ~range,
  tag,
  body,
) =>
  element(
    ~metadata=format(. v).metadata,
    ~avoidsSelection?,
    ~attributes?,
    ~superscript?,
    ~range,
    tag,
    body,
  )->appendOperatorOrFunction(. v, _)

let appendDigit = (v, ~avoidsSelection=?, ~attributes=?, ~superscript=?, ~range, tag, body) =>
  element(
    ~metadata=format(. v).metadata,
    ~avoidsSelection?,
    ~attributes?,
    ~superscript?,
    ~range,
    tag,
    body,
  )->appendDigit(. v, _)

let appendBasePrefix = (v, ~avoidsSelection=?, ~attributes=?, ~superscript=?, ~range, tag, body) =>
  element(
    ~metadata=format(. v).metadata,
    ~avoidsSelection?,
    ~attributes?,
    ~superscript?,
    ~range,
    tag,
    body,
  )->appendBasePrefix(. v, _)

let appendPlaceholder = (v, ~range, ~superscript, placeholder) => {
  let {metadata} = format(. v)
  let phantom = element(
    ~metadata,
    ~attributes=list{selection(~start=fst(range) + 1, ())},
    "mphantom",
    "",
  )
  let symbol = switch placeholder {
  | Some(placeholder) => Mml_Symbol.toMml(placeholder)
  | None =>
    element(
      ~metadata,
      ~attributes=list{(#mathvariant, Mml_Placeholder.mathvariant)},
      Mml_Placeholder.tag,
      Mml_Placeholder.body,
    )
  }
  append(
    v,
    ~attributes=list{(#class, Mml_Placeholder.class)},
    ~superscript?,
    ~range,
    "mrow",
    phantom ++ symbol,
  )
}

let superscriptSuffix = (v, ~attributes=list{}, ~range, tag, body) =>
  modifyLastU(.v, (. last) => {
    let {metadata} = format(. v)
    switch last {
    | Some(last) =>
      let (start, end) = range
      element(
        ~metadata,
        ~attributes=list{selection(~end, ())},
        "msup",
        mml("mrow", last) ++
        element(
          ~metadata,
          ~attributes=list{selection(~avoid=true, ~start, ()), ...attributes},
          tag,
          body,
        ),
      )
    | None =>
      element(
        ~metadata,
        ~range,
        "msup",
        mml(~attributes=Mml_Placeholder.attributes, Mml_Placeholder.tag, Mml_Placeholder.body) ++
        mml(~attributes, tag, body),
      )
    }
  })
