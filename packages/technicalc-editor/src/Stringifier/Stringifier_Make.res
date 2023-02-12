open Stringifier_Types

type lastElementType =
  | NoElement
  | OperatorOrFunction
  | Other

module type Config = {
  let groupingSeparatorU: (. string) => string
  let decimalSeparatorU: (. string, (int, int), bool) => string
  let bracketRangeU: (
    . option<AST.superscript<string>>,
    string,
    (int, int),
    (int, int),
    bool,
  ) => string
  let unpairedOpenBracketU: (. (int, int), bool) => string
  let unpairedCloseBracketU: (. option<AST.superscript<string>>, (int, int), bool) => string
}

module Make = (M: Config): {
  type t
  let make: (. format) => t
  let lastElementType: (. t) => lastElementType
  let format: (. t) => format
  let append: (. t, string) => t
  let appendOperatorOrFunction: (. t, string) => t
  let appendDigit: (. t, string) => t
  let appendDecimalSeparator: (. t, (int, int)) => t
  let appendBasePrefix: (. t, string) => t
  let appendOpenBracket: (. t, (int, int)) => t
  let appendCloseBracket: (. t, (int, int), option<AST.superscript<string>>) => t
  let modifyLastU: (. t, (. option<string>) => string) => t
  let toString: (. t) => string
} => {
  module DigitSeparators = {
    type digitGroupingState =
      | Normal
      | SkipGrouping // After decimal points etc.
      | GroupingDigits({groupingSize: int, numbersRev: list<string>})

    type t = {
      bodyRev: list<string>,
      digitGroupingState: digitGroupingState,
      lastElementType: lastElementType,
    }

    let empty: t = {
      bodyRev: list{},
      digitGroupingState: Normal,
      lastElementType: NoElement,
    }

    let lastElementType = x => x.lastElementType

    %%private(
      let flattenDigits = (~format, v, ~groupingSize, ~numbersRev) => {
        let groupingSeparator = M.groupingSeparatorU(. format.groupingSeparator)
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
            Belt.List.reverseConcat(bodyFwd, v.bodyRev)
          }
        iter(~formattedNumbersFwd=list{}, ~numbersRev)
      }
    )

    %%private(
      let bodyRev = (~format, v) => {
        switch v.digitGroupingState {
        | Normal
        | SkipGrouping =>
          v.bodyRev
        | GroupingDigits({groupingSize, numbersRev}) =>
          flattenDigits(~format, v, ~groupingSize, ~numbersRev)
        }
      }
    )

    let toString = (v, ~format) =>
      bodyRev(v, ~format)->Belt.List.toArray->ArrayUtil.reverseInPlace->StringUtil.join

    %%private(
      let appendWith = (~digitGroupingState=?, ~lastElementType=Other, ~format, v, element) => {
        let digitGroupingState = switch digitGroupingState {
        | Some(digitGroupingState) => digitGroupingState
        | None => Normal
        }
        let bodyRev = list{element, ...bodyRev(v, ~format)}
        {bodyRev, digitGroupingState, lastElementType}
      }
    )

    let append = (~format, v, element) => appendWith(~format, v, element)

    let appendOperatorOrFunction = (~format, v, element) =>
      appendWith(~lastElementType=OperatorOrFunction, ~format, v, element)

    let appendDigit = (~format, v, element) =>
      switch v.digitGroupingState {
      | (Normal | GroupingDigits(_)) as digitGroupingState if format.digitGrouping =>
        let digitGroupingState = switch digitGroupingState {
        | GroupingDigits({groupingSize, numbersRev}) =>
          GroupingDigits({groupingSize, numbersRev: list{element, ...numbersRev}})
        | _ => GroupingDigits({groupingSize: 3, numbersRev: list{element}})
        }
        {bodyRev: v.bodyRev, digitGroupingState, lastElementType: Other}
      | _ as digitGroupingState => appendWith(~digitGroupingState, ~format, v, element)
      }

    let appendDecimalSeparator = (~format, v, range) => {
      let digitGroupingState = SkipGrouping
      let decimalSeparator = M.decimalSeparatorU(. format.decimalSeparator, range, format.metadata)
      appendWith(~digitGroupingState, ~format, v, decimalSeparator)
    }

    let appendBasePrefix = (~format, v, element) => {
      let digitGroupingState = format.digitGrouping
        ? GroupingDigits({groupingSize: 4, numbersRev: list{}})
        : Normal
      appendWith(~digitGroupingState, ~format, v, element)
    }

    let modifyLastU = (~format, v, fn) => {
      let bodyRev = switch bodyRev(~format, v) {
      | list{last, ...rest} => list{fn(. Some(last)), ...rest}
      | list{} => list{fn(. None)}
      }
      {bodyRev, digitGroupingState: Normal, lastElementType: Other}
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

    let format = v => v.format

    let body = ({level0Body, bracketGroups}) =>
      switch bracketGroups {
      | list{{body}, ..._} => body
      | list{} => level0Body
      }

    let transformCurrentGroupWithArgU = ({format, level0Body, bracketGroups}, arg, fn) =>
      switch bracketGroups {
      | list{{body} as bracketGroup, ...rest} => {
          format,
          level0Body,
          bracketGroups: list{{...bracketGroup, body: fn(. format, body, arg)}, ...rest},
        }
      | list{} => {format, level0Body: fn(. format, level0Body, arg), bracketGroups: list{}}
      }

    let appendOpenBracket = (v, openBracketRange) => {
      ...v,
      bracketGroups: list{{openBracketRange, body: DigitSeparators.empty}, ...v.bracketGroups},
    }

    let appendCloseBracket = (v, closeBracketRange, superscript): t =>
      switch v.bracketGroups {
      | list{{openBracketRange, body}, ...nextBracketGroups} =>
        let format = v.format
        let v = {...v, bracketGroups: nextBracketGroups}
        let bracketRange = M.bracketRangeU(.
          superscript,
          DigitSeparators.toString(~format, body),
          openBracketRange,
          closeBracketRange,
          format.metadata,
        )
        transformCurrentGroupWithArgU(v, bracketRange, (. format, a, b) => {
          DigitSeparators.append(~format, a, b)
        })
      | list{} =>
        let unpariedCloseBracket = M.unpairedCloseBracketU(.
          superscript,
          closeBracketRange,
          v.format.metadata,
        )
        transformCurrentGroupWithArgU(v, unpariedCloseBracket, (. format, a, b) => {
          DigitSeparators.append(~format, a, b)
        })
      }

    let toString = v => {
      let rec iter = (~accum, ~bracketGroups) =>
        switch bracketGroups {
        | list{{openBracketRange, body}, ...tail} =>
          let format = v.format
          let body = DigitSeparators.toString(~format, body)
          iter(
            ~accum=M.unpairedOpenBracketU(. openBracketRange, format.metadata) ++ body ++ accum,
            ~bracketGroups=tail,
          )
        | list{} =>
          let format = v.format
          DigitSeparators.toString(~format, v.level0Body) ++ accum
        }
      iter(~accum="", ~bracketGroups=v.bracketGroups)
    }
  }

  type t = BracketGroups.t
  let make = (. format) => BracketGroups.make(~format)
  let lastElementType = (. body) => DigitSeparators.lastElementType(BracketGroups.body(body))
  let format = (. body) => BracketGroups.format(body)
  let append = (. body, element) =>
    BracketGroups.transformCurrentGroupWithArgU(body, element, (. format, a, b) => {
      DigitSeparators.append(~format, a, b)
    })
  let appendOperatorOrFunction = (. body, element) =>
    BracketGroups.transformCurrentGroupWithArgU(body, element, (. format, a, b) => {
      DigitSeparators.appendOperatorOrFunction(~format, a, b)
    })
  let appendDigit = (. body, element) =>
    BracketGroups.transformCurrentGroupWithArgU(body, element, (. format, a, b) => {
      DigitSeparators.appendDigit(~format, a, b)
    })
  let appendDecimalSeparator = (. body, element) =>
    BracketGroups.transformCurrentGroupWithArgU(body, element, (. format, a, b) => {
      DigitSeparators.appendDecimalSeparator(~format, a, b)
    })
  let appendBasePrefix = (. body, element) =>
    BracketGroups.transformCurrentGroupWithArgU(body, element, (. format, a, b) => {
      DigitSeparators.appendBasePrefix(~format, a, b)
    })
  let appendOpenBracket = (. body, range) => BracketGroups.appendOpenBracket(body, range)
  let appendCloseBracket = (. body, range, superscript) =>
    BracketGroups.appendCloseBracket(body, range, superscript)
  let modifyLastU = (. body, element) =>
    BracketGroups.transformCurrentGroupWithArgU(body, element, (. format, a, b) => {
      DigitSeparators.modifyLastU(~format, a, b)
    })
  let toString = (. body) => BracketGroups.toString(body)
}
