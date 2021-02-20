open Mml_Builders;
open Mml_Util;
open Mml_Types;

module MmlPrettifier = {
  type digitGroupingState =
    | GroupingDisabled
    | Normal
    | SkipGrouping /* After decimal points etc. */
    | GroupingDigits({numbersRev: list(string)});

  type lastElementType =
    | NoElement
    | OperatorOrFunction
    | Other;

  type t = {
    locale,
    body: string,
    length: int,
    digitGroupingState,
    lastElementType,
  };

  let make = (~locale, ~digitGrouping) => {
    locale,
    body: "",
    length: 0,
    digitGroupingState: digitGrouping ? Normal : GroupingDisabled,
    lastElementType: NoElement,
  };

  let clear = x =>
    make(
      ~locale=x.locale,
      ~digitGrouping=x.digitGroupingState === GroupingDisabled,
    );

  let lastElementType = x => x.lastElementType;

  let%private flattenDigits = (v, ~numbersRev) => {
    let groupingSeparator =
      switch (v.locale) {
      | English => "<mn>,</mn>"
      | European => "<mn>.</mn>"
      };
    let rec iter = (~numbersRev) =>
      switch (numbersRev) {
      | [c, b, a, ...tail] when tail != [] =>
        iter(~numbersRev=tail) ++ groupingSeparator ++ a ++ b ++ c
      | [number, ...tail] => iter(~numbersRev=tail) ++ number
      | [] => v.body
      };
    iter(~numbersRev);
  };

  let toString = ({digitGroupingState, body} as v) =>
    switch (digitGroupingState) {
    | GroupingDisabled
    | Normal
    | SkipGrouping => body
    | GroupingDigits({numbersRev}) => flattenDigits(v, ~numbersRev)
    };

  let length = v => v.length;

  let%private defaultDigitGroupingState = v =>
    v.digitGroupingState == GroupingDisabled ? GroupingDisabled : Normal;

  let%private appendWith =
              (~digitGroupingState=?, ~lastElementType=Other, v, element) => {
    let digitGroupingState =
      switch (digitGroupingState) {
      | Some(digitGroupingState) => digitGroupingState
      | None => defaultDigitGroupingState(v)
      };
    let {locale} = v;
    let body = toString(v) ++ element;
    let length = v.length + 1;
    {locale, body, length, digitGroupingState, lastElementType};
  };

  let append = (v, element) => appendWith(v, element);

  let appendOperatorOrFunction = (v, element) =>
    appendWith(~lastElementType=OperatorOrFunction, v, element);

  let appendDigit = (v, element) =>
    switch (v.digitGroupingState) {
    | (GroupingDisabled | SkipGrouping) as digitGroupingState =>
      appendWith(~digitGroupingState, v, element)
    | (Normal | GroupingDigits(_)) as digitGroupingState =>
      let numbersRev =
        switch (digitGroupingState) {
        | GroupingDigits({numbersRev}) => numbersRev
        | _ => []
        };
      {
        locale: v.locale,
        body: v.body,
        length: v.length + 1,
        digitGroupingState:
          GroupingDigits({numbersRev: [element, ...numbersRev]}),
        lastElementType: Other,
      };
    };

  let appendDecimalSeparator = (v, range) => {
    let digitGroupingState =
      v.digitGroupingState == GroupingDisabled
        ? GroupingDisabled : SkipGrouping;
    let decimalSeparator =
      switch (v.locale) {
      | English => "."
      | European => ","
      };
    let element = createElementWithRange(range, "mn", decimalSeparator);
    appendWith(~digitGroupingState, v, element);
  };

  let appendBasePrefix = (v, element) => {
    let digitGroupingState =
      v.digitGroupingState == GroupingDisabled
        ? GroupingDisabled : SkipGrouping;
    appendWith(~digitGroupingState, v, element);
  };

  let appendWhitespace = (v, element) =>
    switch (v.lastElementType) {
    | NoElement
    | OperatorOrFunction => v
    | Other => append(v, element)
    };

  let concat = (a, b) => {
    locale: a.locale,
    body: toString(a) ++ toString(b),
    length: a.length + b.length,
    digitGroupingState: defaultDigitGroupingState(a),
    lastElementType:
      b.lastElementType != NoElement ? b.lastElementType : a.lastElementType,
  };

  let map = (v, fn) => {
    locale: v.locale,
    body: toString(v)->fn,
    length: v.length,
    digitGroupingState: defaultDigitGroupingState(v),
    lastElementType: v.lastElementType,
  };
};

module BracketGroups = {
  type bracketGroup = {
    openBracketRange: AST.range,
    body: MmlPrettifier.t,
  };
  type t = {
    level0Body: MmlPrettifier.t,
    bracketGroups: list(bracketGroup),
  };

  let make = (~locale, ~digitGrouping) => {
    level0Body: MmlPrettifier.make(~locale, ~digitGrouping),
    bracketGroups: [],
  };

  let body = ({level0Body, bracketGroups}) =>
    switch (bracketGroups) {
    | [{body}, ..._] => body
    | [] => level0Body
    };

  let transformCurrentGroupWithArg = ({level0Body, bracketGroups}, arg, fn) =>
    switch (bracketGroups) {
    | [{body} as bracketGroup, ...rest] => {
        level0Body,
        bracketGroups: [{...bracketGroup, body: fn(body, arg)}, ...rest],
      }
    | [] => {level0Body: fn(level0Body, arg), bracketGroups: []}
    };

  let appendOpenBracket = (v, openBracketRange) => {
    ...v,
    bracketGroups: [
      {openBracketRange, body: MmlPrettifier.clear(v.level0Body)},
      ...v.bracketGroups,
    ],
  };

  let%private flattenBracketGroup =
              (~attributes=?, v, {openBracketRange, body}) => {
    let openBracket =
      createElementWithRange(~attributes?, openBracketRange, "mo", "(");
    MmlPrettifier.clear(v.level0Body)
    ->MmlPrettifier.append(openBracket)
    ->MmlPrettifier.concat(body);
  };

  let%private invalidAttributes = [
    ("class", "invalid"),
    ("stretchy", "false"),
  ];
  let appendCloseBracket = (v, range, superscript): t =>
    switch (v.bracketGroups) {
    | [closed, ...nextBracketGroups] =>
      // We want the superscript to be over the whole bracket group,
      // not just over the close bracket
      // Every other element works differently to this
      let closeBracket =
        switch (superscript) {
        | Some({AST.superscriptBody: _, index}) =>
          createElementWithRange((fst(range), index), "mo", ")")
        | None => createElementWithRange(range, "mo", ")")
        };

      flattenBracketGroup(v, closed)
      ->MmlPrettifier.append(closeBracket)
      ->MmlPrettifier.map(body =>
          switch (superscript) {
          | Some({AST.superscriptBody}) =>
            createElement(
              ~attributes=[("id", ":" ++ snd(range)->Belt.Int.toString)],
              "msup",
              createElement("mrow", body) ++ superscriptBody,
            )
          | None => createElement("mrow", body)
          }
        )
      ->transformCurrentGroupWithArg(
          {...v, bracketGroups: nextBracketGroups},
          _,
          MmlPrettifier.concat,
        );
    | [] =>
      let attributes = invalidAttributes;
      createElementWithRange(~attributes, ~superscript?, range, "mo", ")")
      ->transformCurrentGroupWithArg(v, _, MmlPrettifier.append);
    };

  let%private flatten = ({level0Body, bracketGroups} as v) => {
    let rec iter = bracketGroups =>
      switch (bracketGroups) {
      | [bracketGroup, ...tail] =>
        let body =
          flattenBracketGroup(~attributes=invalidAttributes, v, bracketGroup);
        MmlPrettifier.concat(iter(tail), body);
      | [] => level0Body
      };
    iter(bracketGroups);
  };

  let toString = (v, range) => {
    let body = flatten(v);
    switch (MmlPrettifier.length(body)) {
    | 0 =>
      createElementWithRange(
        ~attributes=Placeholder.attributes,
        range,
        Placeholder.element,
        Placeholder.body,
      )
    | 1 => MmlPrettifier.toString(body)
    | _ => createElement("mrow", MmlPrettifier.toString(body))
    };
  };
};

let make = BracketGroups.make;
let lastElementType = body =>
  MmlPrettifier.lastElementType(BracketGroups.body(body));
let append = (body, element) =>
  BracketGroups.transformCurrentGroupWithArg(
    body,
    element,
    MmlPrettifier.append,
  );
let appendOperatorOrFunction = (body, element) =>
  BracketGroups.transformCurrentGroupWithArg(
    body,
    element,
    MmlPrettifier.appendOperatorOrFunction,
  );
let appendDigit = (body, element) =>
  BracketGroups.transformCurrentGroupWithArg(
    body,
    element,
    MmlPrettifier.appendDigit,
  );
let appendDecimalSeparator = (body, element) =>
  BracketGroups.transformCurrentGroupWithArg(
    body,
    element,
    MmlPrettifier.appendDecimalSeparator,
  );
let appendBasePrefix = (body, element) =>
  BracketGroups.transformCurrentGroupWithArg(
    body,
    element,
    MmlPrettifier.appendBasePrefix,
  );
let appendWhitespace = (body, width) =>
  BracketGroups.transformCurrentGroupWithArg(
    body,
    width,
    MmlPrettifier.appendWhitespace,
  );
let appendOpenBracket = BracketGroups.appendOpenBracket;
let appendCloseBracket = BracketGroups.appendCloseBracket;
let toString = BracketGroups.toString;
