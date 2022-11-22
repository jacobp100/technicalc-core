open Stringifier_Types

type lastElementType =
  | NoElement
  | OperatorOrFunction
  | Other

module type Config = {
  let groupingSeparatorU: (. locale) => string
  let decimalSeparatorU: (. locale, (int, int)) => string
  let bracketRangeU: (. option<AST.superscript<string>>, string, (int, int), (int, int)) => string
  let unpairedOpenBracketU: (. (int, int)) => string
  let unpairedCloseBracketU: (. option<AST.superscript<string>>, (int, int)) => string
}

module Make = (M: Config): {
  type t
  let make: (. locale, bool) => t
  let lastElementType: (. t) => lastElementType
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
      | GroupingDisabled
      | Normal
      | SkipGrouping // After decimal points etc.
      | GroupingDigits({numbersRev: list<string>})

    type t = {
      locale: locale,
      bodyRev: list<string>,
      digitGroupingState: digitGroupingState,
      lastElementType: lastElementType,
    }

    let make = (~locale, ~digitGrouping) => {
      locale,
      bodyRev: list{},
      digitGroupingState: digitGrouping ? Normal : GroupingDisabled,
      lastElementType: NoElement,
    }

    let clear = x =>
      make(~locale=x.locale, ~digitGrouping=x.digitGroupingState === GroupingDisabled)

    let lastElementType = x => x.lastElementType

    %%private(
      let flattenDigits = (v, ~numbersRev) => {
        let groupingSeparator = M.groupingSeparatorU(. v.locale)
        let rec iter = (~formattedNumbers, ~numbersRev) =>
          switch numbersRev {
          | list{c, b, a, ...tail} if tail != list{} =>
            iter(
              ~formattedNumbers=list{groupingSeparator, a, b, c, ...formattedNumbers},
              ~numbersRev=tail,
            )
          | list{number, ...tail} =>
            iter(~formattedNumbers=list{number, ...formattedNumbers}, ~numbersRev=tail)
          | list{} => Belt.List.reverseConcat(formattedNumbers, v.bodyRev)
          }
        iter(~formattedNumbers=list{}, ~numbersRev)
      }
    )

    %%private(
      let bodyRev = v => {
        switch v.digitGroupingState {
        | GroupingDisabled
        | Normal
        | SkipGrouping =>
          v.bodyRev
        | GroupingDigits({numbersRev}) => flattenDigits(v, ~numbersRev)
        }
      }
    )

    let toString = v => bodyRev(v)->Belt.List.toArray->ArrayUtil.reverseInPlace->StringUtil.join

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
        let bodyRev = list{element, ...bodyRev(v)}
        {
          locale,
          bodyRev,
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
          bodyRev: v.bodyRev,
          digitGroupingState: GroupingDigits({numbersRev: list{element, ...numbersRev}}),
          lastElementType: Other,
        }
      }

    let appendDecimalSeparator = (v, range) => {
      let digitGroupingState =
        v.digitGroupingState == GroupingDisabled ? GroupingDisabled : SkipGrouping
      let decimalSeparator = M.decimalSeparatorU(. v.locale, range)
      appendWith(~digitGroupingState, v, decimalSeparator)
    }

    let appendBasePrefix = (v, element) => {
      let digitGroupingState =
        v.digitGroupingState == GroupingDisabled ? GroupingDisabled : SkipGrouping
      appendWith(~digitGroupingState, v, element)
    }

    let modifyLastU = ({locale} as v, fn) => {
      let bodyRev = switch bodyRev(v) {
      | list{last, ...rest} => list{fn(. Some(last)), ...rest}
      | list{} => list{fn(. None)}
      }
      {
        locale,
        bodyRev,
        digitGroupingState: Normal,
        lastElementType: Other,
      }
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
      @as("hd") level0Body: DigitSeparators.t,
      @as("tl") bracketGroups: list<bracketGroup>,
    }

    let make = (~locale, ~digitGrouping) => {
      level0Body: DigitSeparators.make(~locale, ~digitGrouping),
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
        {openBracketRange, body: DigitSeparators.clear(v.level0Body)},
        ...v.bracketGroups,
      },
    }

    let appendCloseBracket = (v, closeBracketRange, superscript): t =>
      switch v.bracketGroups {
      | list{{openBracketRange, body}, ...nextBracketGroups} =>
        let v = {...v, bracketGroups: nextBracketGroups}
        let bracketRange = M.bracketRangeU(.
          superscript,
          DigitSeparators.toString(body),
          openBracketRange,
          closeBracketRange,
        )
        transformCurrentGroupWithArgU(v, bracketRange, (. a, b) => {
          DigitSeparators.append(a, b)
        })
      | list{} =>
        let unpariedCloseBracket = M.unpairedCloseBracketU(. superscript, closeBracketRange)
        transformCurrentGroupWithArgU(v, unpariedCloseBracket, (. a, b) => {
          DigitSeparators.append(a, b)
        })
      }

    let toString = v => {
      let rec iter = (~accum, ~bracketGroups) =>
        switch bracketGroups {
        | list{{openBracketRange, body}, ...tail} =>
          let body = DigitSeparators.toString(body)
          iter(
            ~accum=M.unpairedOpenBracketU(. openBracketRange) ++ body ++ accum,
            ~bracketGroups=tail,
          )
        | list{} => DigitSeparators.toString(v.level0Body) ++ accum
        }
      iter(~accum="", ~bracketGroups=v.bracketGroups)
    }
  }

  type t = BracketGroups.t
  let make = (. locale, digitGrouping) => BracketGroups.make(~locale, ~digitGrouping)
  let lastElementType = (. body) => DigitSeparators.lastElementType(BracketGroups.body(body))
  let append = (. body, element) =>
    BracketGroups.transformCurrentGroupWithArgU(body, element, (. a, b) => {
      DigitSeparators.append(a, b)
    })
  let appendOperatorOrFunction = (. body, element) =>
    BracketGroups.transformCurrentGroupWithArgU(body, element, (. a, b) => {
      DigitSeparators.appendOperatorOrFunction(a, b)
    })
  let appendDigit = (. body, element) =>
    BracketGroups.transformCurrentGroupWithArgU(body, element, (. a, b) => {
      DigitSeparators.appendDigit(a, b)
    })
  let appendDecimalSeparator = (. body, element) =>
    BracketGroups.transformCurrentGroupWithArgU(body, element, (. a, b) => {
      DigitSeparators.appendDecimalSeparator(a, b)
    })
  let appendBasePrefix = (. body, element) =>
    BracketGroups.transformCurrentGroupWithArgU(body, element, (. a, b) => {
      DigitSeparators.appendBasePrefix(a, b)
    })
  let appendOpenBracket = (. body, range) => BracketGroups.appendOpenBracket(body, range)
  let appendCloseBracket = (. body, range, superscript) =>
    BracketGroups.appendCloseBracket(body, range, superscript)
  let modifyLastU = (. body, element) =>
    BracketGroups.transformCurrentGroupWithArgU(body, element, (. a, b) => {
      DigitSeparators.modifyLastU(a, b)
    })
  let toString = (. body) => BracketGroups.toString(body)
}
