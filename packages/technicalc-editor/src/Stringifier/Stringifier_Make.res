open Stringifier_Types

type lastElementFlag =
  | @as(0) NoElement
  | @as(1) Other

module type Config = {
  type elementFlag
  let groupingSeparator: string => string
  let decimalSeparator: (string, (int, int), bool) => string
  let bracketRange: (
    option<AST.superscript<string>>,
    string,
    (int, int),
    (int, int),
    bool,
  ) => string
  let unpairedOpenBracket: ((int, int), bool) => string
  let unpairedCloseBracket: (option<AST.superscript<string>>, (int, int), bool) => string
}

module Make = (M: Config): {
  type t
  let make: format => t
  let lastElementFlag: t => option<M.elementFlag>
  let isEmpty: t => bool
  let format: t => format
  let append: (t, string) => t
  let appendWithFlag: (t, M.elementFlag, string) => t
  let appendDigit: (t, string) => t
  let appendDecimalSeparator: (t, (int, int)) => t
  let appendBasePrefix: (t, string) => t
  let appendOpenBracket: (t, (int, int)) => t
  let appendCloseBracket: (t, (int, int), option<AST.superscript<string>>) => t
  let modifyLast: (t, option<string> => string) => t
  let toString: t => string
} => {
  module DigitSeparators = {
    type digitGroupingState =
      | @as(0) Normal
      | @as(1) SkipGrouping // After decimal points etc.
      | @as(2) GroupingDigits({groupingSize: int, numbersRev: list<string>})

    type t = {
      bodyRev: list<string>,
      digitGroupingState: digitGroupingState,
      lastElementFlag: option<M.elementFlag>,
    }

    let empty: t = {
      bodyRev: list{},
      digitGroupingState: Normal,
      lastElementFlag: None,
    }

    let isEmpty = x => x.bodyRev == list{} && x.digitGroupingState == Normal

    let lastElementFlag = x => x.lastElementFlag

    %%private(
      let flattenDigits = (~format, x, ~groupingSize, ~numbersRev) => {
        let groupingSeparator = M.groupingSeparator(format.groupingSeparator)
        let rec iter = (~formattedNumbersFwd, ~numbersRev) =>
          switch Belt.List.splitAt(numbersRev, groupingSize) {
          | Some((groupRev, numbersRev)) if numbersRev != list{} =>
            let formattedNumbersFwd = list{
              groupingSeparator,
              ...Belt.List.reverseConcat(groupRev, formattedNumbersFwd),
            }
            iter(~formattedNumbersFwd, ~numbersRev)
          | _ =>
            let bodyFwd = Belt.List.reverseConcat(numbersRev, formattedNumbersFwd)
            Belt.List.reverseConcat(bodyFwd, x.bodyRev)
          }
        iter(~formattedNumbersFwd=list{}, ~numbersRev)
      }
    )

    %%private(
      let bodyRev = (~format, x) => {
        switch x.digitGroupingState {
        | Normal
        | SkipGrouping =>
          x.bodyRev
        | GroupingDigits({groupingSize, numbersRev}) =>
          flattenDigits(~format, x, ~groupingSize, ~numbersRev)
        }
      }
    )

    let toString = (x, ~format) =>
      bodyRev(x, ~format)->Belt.List.toArray->ArrayUtil.reverseInPlace->StringUtil.join

    %%private(
      let appendWith = (~digitGroupingState=?, ~flag as lastElementFlag=?, ~format, x, element) => {
        let digitGroupingState = switch digitGroupingState {
        | Some(digitGroupingState) => digitGroupingState
        | None => Normal
        }
        let bodyRev = list{element, ...bodyRev(x, ~format)}
        {bodyRev, digitGroupingState, lastElementFlag}
      }
    )

    let append = (~flag=?, ~format, x, element) => appendWith(~flag?, ~format, x, element)

    let appendDigit = (~format, x, element) =>
      switch x.digitGroupingState {
      | (Normal | GroupingDigits(_)) as digitGroupingState if format.digitGrouping =>
        let digitGroupingState = switch digitGroupingState {
        | GroupingDigits({groupingSize, numbersRev}) =>
          GroupingDigits({groupingSize, numbersRev: list{element, ...numbersRev}})
        | _ => GroupingDigits({groupingSize: 3, numbersRev: list{element}})
        }
        {bodyRev: x.bodyRev, digitGroupingState, lastElementFlag: None}
      | _ as digitGroupingState => appendWith(~digitGroupingState, ~format, x, element)
      }

    let appendDecimalSeparator = (~format, x, range) => {
      let digitGroupingState = SkipGrouping
      let decimalSeparator = M.decimalSeparator(format.decimalSeparator, range, format.metadata)
      appendWith(~digitGroupingState, ~format, x, decimalSeparator)
    }

    let appendBasePrefix = (~format, x, element) => {
      let digitGroupingState = format.digitGrouping
        ? GroupingDigits({groupingSize: 4, numbersRev: list{}})
        : Normal
      appendWith(~digitGroupingState, ~format, x, element)
    }

    let modifyLast = (~format, x, fn) => {
      let bodyRev = switch bodyRev(~format, x) {
      | list{last, ...rest} => list{fn(Some(last)), ...rest}
      | list{} => list{fn(None)}
      }
      {bodyRev, digitGroupingState: Normal, lastElementFlag: None}
    }
  }

  module BracketGroups = {
    type bracketGroup = {
      openBracketRange: AST.range,
      body: DigitSeparators.t,
    }
    // List with typed guarantee it has at least one element
    // JS encoding matches regular list
    type t = {
      format: format,
      level0Body: DigitSeparators.t,
      bracketGroups: list<bracketGroup>,
    }

    let make = (~format) => {
      format,
      level0Body: DigitSeparators.empty,
      bracketGroups: list{},
    }

    let isEmpty = x => x.bracketGroups == list{} && DigitSeparators.isEmpty(x.level0Body)

    let format = x => x.format

    let body = ({level0Body, bracketGroups}) =>
      switch bracketGroups {
      | list{{body}, ..._} => body
      | list{} => level0Body
      }

    let transformCurrentGroupWithArg = ({format, level0Body, bracketGroups}, arg, fn) =>
      switch bracketGroups {
      | list{{body} as bracketGroup, ...rest} => {
          format,
          level0Body,
          bracketGroups: list{{...bracketGroup, body: fn(format, body, arg)}, ...rest},
        }
      | list{} => {format, level0Body: fn(format, level0Body, arg), bracketGroups: list{}}
      }

    let appendOpenBracket = (x, openBracketRange) => {
      ...x,
      bracketGroups: list{{openBracketRange, body: DigitSeparators.empty}, ...x.bracketGroups},
    }

    let appendCloseBracket = (x, closeBracketRange, superscript): t =>
      switch x.bracketGroups {
      | list{{openBracketRange, body}, ...nextBracketGroups} =>
        let format = x.format
        let x = {...x, bracketGroups: nextBracketGroups}
        let bracketRange = M.bracketRange(
          superscript,
          DigitSeparators.toString(~format, body),
          openBracketRange,
          closeBracketRange,
          format.metadata,
        )
        transformCurrentGroupWithArg(x, bracketRange, (format, a, b) => {
          DigitSeparators.append(~format, a, b)
        })
      | list{} =>
        let unpariedCloseBracket = M.unpairedCloseBracket(
          superscript,
          closeBracketRange,
          x.format.metadata,
        )
        transformCurrentGroupWithArg(x, unpariedCloseBracket, (format, a, b) => {
          DigitSeparators.append(~format, a, b)
        })
      }

    let toString = x => {
      let rec iter = (~accum, ~bracketGroups) =>
        switch bracketGroups {
        | list{{openBracketRange, body}, ...tail} =>
          let format = x.format
          let body = DigitSeparators.toString(~format, body)
          iter(
            ~accum=M.unpairedOpenBracket(openBracketRange, format.metadata) ++ body ++ accum,
            ~bracketGroups=tail,
          )
        | list{} =>
          let format = x.format
          DigitSeparators.toString(~format, x.level0Body) ++ accum
        }
      iter(~accum="", ~bracketGroups=x.bracketGroups)
    }
  }

  type t = BracketGroups.t
  let make = format => BracketGroups.make(~format)
  let lastElementFlag = body => DigitSeparators.lastElementFlag(BracketGroups.body(body))
  let isEmpty = body => BracketGroups.isEmpty(body)
  let format = body => BracketGroups.format(body)
  let append = (body, element) =>
    BracketGroups.transformCurrentGroupWithArg(body, element, (format, a, b) => {
      DigitSeparators.append(~format, a, b)
    })
  let appendWithFlag = (body, flag, element) =>
    BracketGroups.transformCurrentGroupWithArg(body, element, (format, a, b) => {
      DigitSeparators.append(~flag, ~format, a, b)
    })
  let appendDigit = (body, element) =>
    BracketGroups.transformCurrentGroupWithArg(body, element, (format, a, b) => {
      DigitSeparators.appendDigit(~format, a, b)
    })
  let appendDecimalSeparator = (body, element) =>
    BracketGroups.transformCurrentGroupWithArg(body, element, (format, a, b) => {
      DigitSeparators.appendDecimalSeparator(~format, a, b)
    })
  let appendBasePrefix = (body, element) =>
    BracketGroups.transformCurrentGroupWithArg(body, element, (format, a, b) => {
      DigitSeparators.appendBasePrefix(~format, a, b)
    })
  let appendOpenBracket = (body, range) => BracketGroups.appendOpenBracket(body, range)
  let appendCloseBracket = (body, range, superscript) =>
    BracketGroups.appendCloseBracket(body, range, superscript)
  let modifyLast = (body, element) =>
    BracketGroups.transformCurrentGroupWithArg(body, element, (format, a, b) => {
      DigitSeparators.modifyLast(~format, a, b)
    })
  let toString = body => BracketGroups.toString(body)
}
