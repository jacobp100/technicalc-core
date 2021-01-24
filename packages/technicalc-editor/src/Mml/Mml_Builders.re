let createElement = (~attributes=[], element, body) => {
  let attributes =
    attributes
    ->Belt.List.map(((p, v)) => p ++ "=\"" ++ v ++ "\"")
    ->Belt.List.toArray
    ->StringUtil.joinWith(" ");
  let head =
    switch (attributes) {
    | "" => "<" ++ element ++ ">"
    | attributes => "<" ++ element ++ " " ++ attributes ++ ">"
    };
  head ++ body ++ "</" ++ element ++ ">";
};

let createSuperscript =
    (~containerAttributes=[], ~attributes=[], superscript, element, body) => {
  let {AST.superscriptBody, index: s} = superscript;
  let base =
    createElement(
      ~attributes=[("id", ":" ++ string_of_int(s)), ...attributes],
      element,
      body,
    );
  createElement(
    ~attributes=containerAttributes,
    "msup",
    base ++ superscriptBody,
  );
};

let createElementWithRange =
    (~attributes=[], ~superscript=?, (i, i'), element, body) => {
  let idAttribute = ("id", string_of_int(i) ++ ":" ++ string_of_int(i'));

  switch (superscript) {
  | None =>
    createElement(~attributes=[idAttribute, ...attributes], element, body)
  | Some(superscript) =>
    createSuperscript(
      ~containerAttributes=[idAttribute],
      ~attributes,
      superscript,
      element,
      body,
    )
  };
};
