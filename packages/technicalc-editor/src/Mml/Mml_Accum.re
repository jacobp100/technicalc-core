open Mml_Builders;

module DigitGroups = {
  type state =
    | GroupingDisabled
    | Normal
    | SkipGrouping /* After decimal points etc. */
    | GroupingDigits({numbersRev: list(string)});

  type t = {
    state,
    body: string,
    length: int,
  };

  let make = (~digitGrouping) => {
    state: digitGrouping ? Normal : GroupingDisabled,
    body: "",
    length: 0,
  };

  let digitGroupingEnabled = x => x.state === GroupingDisabled;

  let%private rec flattenDigits = (~body, ~numbersRev) =>
    switch (numbersRev) {
    | [c, b, a, ...tail] when tail != [] =>
      flattenDigits(~body, ~numbersRev=tail) ++ "<mn>,</mn>" ++ a ++ b ++ c
    | [number, ...tail] => flattenDigits(~body, ~numbersRev=tail) ++ number
    | [] => body
    };

  let toString = ({state, body}) =>
    switch (state) {
    | GroupingDisabled
    | Normal
    | SkipGrouping => body
    | GroupingDigits({numbersRev}) => flattenDigits(~body, ~numbersRev)
    };

  let length = v => v.length;

  let append = (v, element) => {
    state: v.state == GroupingDisabled ? GroupingDisabled : Normal,
    body: toString(v) ++ element,
    length: v.length + 1,
  };

  let appendDigit = ({state, body, length}, element) => {
    let length = length + 1;
    switch (state) {
    | GroupingDisabled
    | SkipGrouping => {state, body: body ++ element, length}
    | Normal => {
        state: GroupingDigits({numbersRev: [element]}),
        body,
        length,
      }
    | GroupingDigits({numbersRev}) => {
        state: GroupingDigits({numbersRev: [element, ...numbersRev]}),
        body,
        length,
      }
    };
  };

  let appendDecimalSeparator = (v, element) => {
    state: v.state == GroupingDisabled ? GroupingDisabled : SkipGrouping,
    body: toString(v) ++ element,
    length: v.length + 1,
  };

  let appendBasePrefix = appendDecimalSeparator;

  let concat = (a, b) => {
    state: a.state == GroupingDisabled ? GroupingDisabled : Normal,
    body: toString(a) ++ toString(b),
    length: a.length + b.length,
  };

  let map = (v, fn) => {
    state: v.state == GroupingDisabled ? GroupingDisabled : Normal,
    body: toString(v)->fn,
    length: v.length,
  };
};

module BracketGroups = {
  type bracketGroup = {
    openBracketRange: AST.range,
    body: DigitGroups.t,
  };
  type t = {
    level0Body: DigitGroups.t,
    bracketGroups: list(bracketGroup),
  };

  let make = (~digitGrouping) => {
    level0Body: DigitGroups.make(~digitGrouping),
    bracketGroups: [],
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
      {
        openBracketRange,
        body:
          DigitGroups.digitGroupingEnabled(v.level0Body)
          ->DigitGroups.make(~digitGrouping=_),
      },
      ...v.bracketGroups,
    ],
  };

  let%private flattenBracketGroup =
              (~attributes=?, v, {openBracketRange, body}) => {
    let openBracket =
      elementWithRange(~attributes?, "mo", openBracketRange, "(");
    DigitGroups.digitGroupingEnabled(v.level0Body)
    ->DigitGroups.make(~digitGrouping=_)
    ->DigitGroups.append(openBracket)
    ->DigitGroups.concat(body);
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
          elementWithRange("mo", (fst(range), index), ")")
        | None => elementWithRange("mo", range, ")")
        };

      flattenBracketGroup(v, closed)
      ->DigitGroups.append(closeBracket)
      ->DigitGroups.map(body =>
          switch (superscript) {
          | Some({AST.superscriptBody}) =>
            createElement(
              ~attributes=[("id", ":" ++ snd(range)->string_of_int)],
              "msup",
              createElement("mrow", body) ++ superscriptBody,
            )
          | None => createElement("mrow", body)
          }
        )
      ->transformCurrentGroupWithArg(
          {...v, bracketGroups: nextBracketGroups},
          _,
          DigitGroups.concat,
        );
    | [] =>
      let attributes = invalidAttributes;
      elementWithRange(~attributes, ~superscript?, "mo", range, ")")
      ->transformCurrentGroupWithArg(v, _, DigitGroups.append);
    };

  let%private flatten = ({level0Body, bracketGroups} as v) => {
    let rec iter = bracketGroups =>
      switch (bracketGroups) {
      | [bracketGroup, ...tail] =>
        let body =
          flattenBracketGroup(~attributes=invalidAttributes, v, bracketGroup);
        DigitGroups.concat(iter(tail), body);
      | [] => level0Body
      };
    iter(bracketGroups);
  };

  let toString = (v, range) => {
    let body = flatten(v);
    switch (DigitGroups.length(body)) {
    | 0 =>
      elementWithRange(
        ~attributes=Placeholder.attributes,
        Placeholder.element,
        range,
        Placeholder.body,
      )
    | 1 => DigitGroups.toString(body)
    | _ => createElement("mrow", DigitGroups.toString(body))
    };
  };
};

let make = BracketGroups.make;
let append = (body, element) =>
  BracketGroups.transformCurrentGroupWithArg(
    body,
    element,
    DigitGroups.append,
  );
let appendDigit = (body, element) =>
  BracketGroups.transformCurrentGroupWithArg(
    body,
    element,
    DigitGroups.appendDigit,
  );
let appendDecimalSeparator = (body, element) =>
  BracketGroups.transformCurrentGroupWithArg(
    body,
    element,
    DigitGroups.appendDecimalSeparator,
  );
let appendBasePrefix = (body, element) =>
  BracketGroups.transformCurrentGroupWithArg(
    body,
    element,
    DigitGroups.appendBasePrefix,
  );
let appendOpenBracket = BracketGroups.appendOpenBracket;
let appendCloseBracket = BracketGroups.appendCloseBracket;
let toString = BracketGroups.toString;
