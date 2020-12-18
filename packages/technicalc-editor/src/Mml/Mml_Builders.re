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

let elementWithRange =
    (~attributes=[], ~superscript=?, element, (i, i'), body) =>
  switch (superscript) {
  | None =>
    let attributes = [
      ("id", string_of_int(i) ++ ":" ++ string_of_int(i')),
      ...attributes,
    ];
    createElement(~attributes, element, body);
  | Some({AST.superscriptBody, index: s}) =>
    let base =
      createElement(
        ~attributes=[("id", ":" ++ string_of_int(s)), ...attributes],
        element,
        body,
      );
    createElement(
      ~attributes=[("id", string_of_int(i) ++ ":" ++ string_of_int(i'))],
      "msup",
      base ++ superscriptBody,
    );
  };

module Placeholder = {
  let attributes = [("class", "placeholder"), ("mathvariant", "normal")];
  let element = "mi";
  let body = "&#x25a1;";
};

let xSetRow = value =>
  createElement(
    "mrow",
    createElement("mi", "x") ++ createElement("mo", "=") ++ value,
  );
