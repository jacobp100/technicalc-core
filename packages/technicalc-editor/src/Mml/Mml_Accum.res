open Mml_Builders
open Mml_Util
open Mml_Types

type lastElementType =
  | NoElement
  | OperatorOrFunction
  | Other

include (
  {
    module MmlPrettifier = {
      type digitGroupingState =
        | GroupingDisabled
        | Normal
        | SkipGrouping // After decimal points etc.
        | GroupingDigits({numbersRev: list<string>})

      // type lastElementType =
      //   | NoElement
      //   | OperatorOrFunction
      //   | Other

      type t = {
        locale: locale,
        body: string,
        length: int,
        digitGroupingState: digitGroupingState,
        lastElementType: lastElementType,
      }

      let make = (~locale, ~digitGrouping) => {
        locale,
        body: "",
        length: 0,
        digitGroupingState: digitGrouping ? Normal : GroupingDisabled,
        lastElementType: NoElement,
      }

      let clear = x =>
        make(~locale=x.locale, ~digitGrouping=x.digitGroupingState === GroupingDisabled)

      let lastElementType = x => x.lastElementType

      %%private(
        let flattenDigits = (v, ~numbersRev) => {
          let groupingSeparator = switch v.locale {
          | English => "<mn>,</mn>"
          | European => "<mn>.</mn>"
          }
          let rec iter = (~formattedNumbers, ~numbersRev) =>
            switch numbersRev {
            | list{c, b, a, ...tail} if tail != list{} =>
              iter(
                ~formattedNumbers=groupingSeparator ++ a ++ b ++ c ++ formattedNumbers,
                ~numbersRev=tail,
              )
            | list{number, ...tail} =>
              iter(~formattedNumbers=number ++ formattedNumbers, ~numbersRev=tail)
            | list{} => v.body ++ formattedNumbers
            }
          iter(~formattedNumbers="", ~numbersRev)
        }
      )

      let toString = ({digitGroupingState, body} as v) =>
        switch digitGroupingState {
        | GroupingDisabled
        | Normal
        | SkipGrouping => body
        | GroupingDigits({numbersRev}) => flattenDigits(v, ~numbersRev)
        }

      let length = v => v.length

      %%private(
        let defaultDigitGroupingState = v =>
          v.digitGroupingState == GroupingDisabled ? GroupingDisabled : Normal
      )

      %%private(
        let appendWith = (~digitGroupingState=?, ~lastElementType=Other, v, element) => {
          let digitGroupingState = switch digitGroupingState {
          | Some(digitGroupingState) => digitGroupingState
          | None => defaultDigitGroupingState(v)
          }
          let {locale} = v
          let body = toString(v) ++ element
          let length = v.length + 1
          {
            locale,
            body,
            length,
            digitGroupingState,
            lastElementType,
          }
        }
      )

      let append = (v, element) => appendWith(v, element)

      let appendOperatorOrFunction = (v, element) =>
        appendWith(~lastElementType=OperatorOrFunction, v, element)

      let appendDigit = (v, element) =>
        switch v.digitGroupingState {
        | (GroupingDisabled | SkipGrouping) as digitGroupingState =>
          appendWith(~digitGroupingState, v, element)
        | (Normal | GroupingDigits(_)) as digitGroupingState =>
          let numbersRev = switch digitGroupingState {
          | GroupingDigits({numbersRev}) => numbersRev
          | _ => list{}
          }
          {
            locale: v.locale,
            body: v.body,
            length: v.length + 1,
            digitGroupingState: GroupingDigits({numbersRev: list{element, ...numbersRev}}),
            lastElementType: Other,
          }
        }

      let appendDecimalSeparator = (v, range) => {
        let digitGroupingState =
          v.digitGroupingState == GroupingDisabled ? GroupingDisabled : SkipGrouping
        let decimalSeparator = switch v.locale {
        | English => "."
        | European => ","
        }
        let element = createElementWithRange(range, "mn", decimalSeparator)
        appendWith(~digitGroupingState, v, element)
      }

      let appendBasePrefix = (v, element) => {
        let digitGroupingState =
          v.digitGroupingState == GroupingDisabled ? GroupingDisabled : SkipGrouping
        appendWith(~digitGroupingState, v, element)
      }

      let concat = (a, b) => {
        locale: a.locale,
        body: toString(a) ++ toString(b),
        length: a.length + b.length,
        digitGroupingState: defaultDigitGroupingState(a),
        lastElementType: b.lastElementType != NoElement ? b.lastElementType : a.lastElementType,
      }

      let mapU = (v, fn) => {
        locale: v.locale,
        body: fn(. toString(v)),
        length: v.length,
        digitGroupingState: defaultDigitGroupingState(v),
        lastElementType: v.lastElementType,
      }
    }

    module BracketGroups = {
      type bracketGroup = {
        openBracketRange: AST.range,
        body: MmlPrettifier.t,
      }
      // List with typed guarantee it has at least one element
      // JS encoding matches regular list
      type t = {
        @as("hd") level0Body: MmlPrettifier.t,
        @as("tl") bracketGroups: list<bracketGroup>,
      }

      let make = (~locale, ~digitGrouping) => {
        level0Body: MmlPrettifier.make(~locale, ~digitGrouping),
        bracketGroups: list{},
      }

      let body = ({level0Body, bracketGroups}) =>
        switch bracketGroups {
        | list{{body}, ..._} => body
        | list{} => level0Body
        }

      let transformCurrentGroupWithArgU = ({level0Body, bracketGroups}, arg, fn) =>
        switch bracketGroups {
        | list{{body} as bracketGroup, ...rest} => {
            level0Body,
            bracketGroups: list{{...bracketGroup, body: fn(. body, arg)}, ...rest},
          }
        | list{} => {level0Body: fn(. level0Body, arg), bracketGroups: list{}}
        }

      let appendOpenBracket = (v, openBracketRange) => {
        ...v,
        bracketGroups: list{
          {openBracketRange, body: MmlPrettifier.clear(v.level0Body)},
          ...v.bracketGroups,
        },
      }

      %%private(
        let flattenBracketGroup = (~attributes=?, v, {openBracketRange, body}) => {
          let openBracket = createElementWithRange(~attributes?, openBracketRange, "mo", "(")
          MmlPrettifier.clear(v.level0Body)
          ->MmlPrettifier.append(openBracket)
          ->MmlPrettifier.concat(body)
        }
      )

      %%private(let invalidAttributes = list{("class", "invalid"), ("stretchy", "false")})
      let appendCloseBracket = (v, range, superscript): t =>
        switch v.bracketGroups {
        | list{closed, ...nextBracketGroups} =>
          // We want the superscript to be over the whole bracket group,
          // not just over the close bracket
          // Every other element works differently to this
          let closeBracket = switch superscript {
          | Some({AST.superscriptBody: _, index}) =>
            createElementWithRange((fst(range), index), "mo", ")")
          | None => createElementWithRange(range, "mo", ")")
          }

          flattenBracketGroup(v, closed)
          ->MmlPrettifier.append(closeBracket)
          ->MmlPrettifier.mapU((. body) => {
            switch superscript {
            | Some({AST.superscriptBody: superscriptBody}) =>
              createElement(
                ~attributes=list{("id", ":" ++ snd(range)->Belt.Int.toString)},
                "msup",
                createElement("mrow", body) ++ superscriptBody,
              )
            | None => createElement("mrow", body)
            }
          })
          ->transformCurrentGroupWithArgU({...v, bracketGroups: nextBracketGroups}, _, (. a, b) => {
            MmlPrettifier.concat(a, b)
          })
        | list{} =>
          createElementWithRange(
            ~attributes=invalidAttributes,
            ~superscript?,
            range,
            "mo",
            ")",
          )->transformCurrentGroupWithArgU(v, _, (. a, b) => MmlPrettifier.append(a, b))
        }

      %%private(
        let flatten = ({level0Body, bracketGroups} as v) => {
          let rec iter = (~accum, ~bracketGroups) =>
            switch bracketGroups {
            | list{bracketGroup, ...tail} =>
              let body = flattenBracketGroup(~attributes=invalidAttributes, v, bracketGroup)
              iter(~accum=MmlPrettifier.concat(accum, body), ~bracketGroups=tail)
            | list{} => accum
            }
          iter(~accum=level0Body, ~bracketGroups)
        }
      )

      let toString = (v, range) => {
        let body = flatten(v)
        switch MmlPrettifier.length(body) {
        | 0 =>
          createElementWithRange(
            ~attributes=Placeholder.attributes,
            range,
            Placeholder.element,
            Placeholder.body,
          )
        | 1 => MmlPrettifier.toString(body)
        | _ => createElement("mrow", MmlPrettifier.toString(body))
        }
      }
    }

    type t = BracketGroups.t
    let make = BracketGroups.make
    let lastElementType = body => MmlPrettifier.lastElementType(BracketGroups.body(body))
    let append = (body, element) =>
      BracketGroups.transformCurrentGroupWithArgU(body, element, (. a, b) => {
        MmlPrettifier.append(a, b)
      })
    let appendOperatorOrFunction = (body, element) =>
      BracketGroups.transformCurrentGroupWithArgU(body, element, (. a, b) => {
        MmlPrettifier.appendOperatorOrFunction(a, b)
      })
    let appendDigit = (body, element) =>
      BracketGroups.transformCurrentGroupWithArgU(body, element, (. a, b) => {
        MmlPrettifier.appendDigit(a, b)
      })
    let appendDecimalSeparator = (body, element) =>
      BracketGroups.transformCurrentGroupWithArgU(body, element, (. a, b) => {
        MmlPrettifier.appendDecimalSeparator(a, b)
      })
    let appendBasePrefix = (body, element) =>
      BracketGroups.transformCurrentGroupWithArgU(body, element, (. a, b) => {
        MmlPrettifier.appendBasePrefix(a, b)
      })
    let appendOpenBracket = BracketGroups.appendOpenBracket
    let appendCloseBracket = BracketGroups.appendCloseBracket
    let toString = BracketGroups.toString
  }: {
    type t
    let make: (~locale: TechniCalcEditor.Mml_Types.locale, ~digitGrouping: bool) => t
    let lastElementType: t => lastElementType
    let append: (t, string) => t
    let appendOperatorOrFunction: (t, string) => t
    let appendDigit: (t, string) => t
    let appendDecimalSeparator: (t, AST.range) => t
    let appendBasePrefix: (t, string) => t
    let appendOpenBracket: (t, AST.range) => t
    let appendCloseBracket: (t, AST.range, option<AST.superscript<string>>) => t
    let toString: (t, AST.range) => string
  }
)
