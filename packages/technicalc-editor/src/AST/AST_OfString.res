open AST_Types

%%private(
  let elementExn = x =>
    switch x {
    | '+' => Add
    | '-' => Sub
    | '*' => Mul
    | '/' => Div
    | '!' => Factorial
    | '%' => Percent
    | '(' => OpenBracket
    | ')' => CloseBracketS
    | '.' => DecimalSeparator
    | '0' => N0_S
    | '1' => N1_S
    | '2' => N2_S
    | '3' => N3_S
    | '4' => N4_S
    | '5' => N5_S
    | '6' => N6_S
    | '7' => N7_S
    | '8' => N8_S
    | '9' => N9_S
    | 'A' | 'a' => NA_S
    | 'B' | 'b' => NB_S
    | 'C' | 'c' => NC_S
    | 'D' | 'd' => ND_S
    | 'E' | 'e' => NE_S
    | 'F' | 'f' => NF_S
    | _ => assert(false)
    }
)

%%private(
  let rec iter = (~didInsertHex=false, ~elementsRev=list{}, input) =>
    switch input {
    | list{'0', 'b', ...rest} =>
      iter(~didInsertHex=false, ~elementsRev=list{Bin, ...elementsRev}, rest)
    | list{'0', 'o', ...rest} =>
      iter(~didInsertHex=false, ~elementsRev=list{Oct, ...elementsRev}, rest)
    | list{'0', 'x', ...rest} =>
      iter(~didInsertHex=true, ~elementsRev=list{Hex, ...elementsRev}, rest)
    | list{'^', ...rest} =>
      let element = Superscript1
      parseArg(~elementsRev=list{element, ...elementsRev}, rest)
    | list{'*' | 'x', '1', '0', '^', ...rest}
    | list{'*' | 'x', ' ', '1', '0', '^', ...rest}
    | list{'*' | 'x', '1', '0', ' ', '^', ...rest}
    | list{'*' | 'x', ' ', '1', '0', ' ', '^', ...rest} =>
      let element = Magnitude1
      parseArg(~elementsRev=list{element, ...elementsRev}, rest)
    | list{'e' | 'E', ...rest} if !didInsertHex =>
      let element = Magnitude1
      parseArg(~elementsRev=list{element, ...elementsRev}, rest)
    | list{'a' .. 'f' as digit, ...rest}
    | list{'A' .. 'F' as digit, ...rest} if didInsertHex =>
      let element = elementExn(digit)
      iter(~didInsertHex, ~elementsRev=list{element, ...elementsRev}, rest)
    | list{('0' .. '9' | '.') as digit, ...rest} =>
      let element = elementExn(digit)
      iter(~didInsertHex, ~elementsRev=list{element, ...elementsRev}, rest)
    | list{('+' | '-' | '*' | '/' | '!' | '%' | '(' | ')') as operatorLike, ...rest} =>
      let element = elementExn(operatorLike)
      iter(~didInsertHex=false, ~elementsRev=list{element, ...elementsRev}, rest)
    | list{_, ...rest} => iter(~didInsertHex, ~elementsRev, rest)
    | list{} => elementsRev
    }
  and parseArg = (~elementsRev, input) => {
    let next = iter

    let rec iter = (~inUnaryPosition, ~bracketLevel, ~stackRev, input) =>
      switch input {
      | list{('+' | '-' | '*' | '/') as char, ...rest}
        if bracketLevel == 0 && !(inUnaryPosition && (char == '+' || char == '-')) =>
        let elementsRev = iterStackRev(~stackRev, ~elementsRev)
        let elementsRev = list{elementExn(char), ...elementsRev}
        next(~elementsRev, rest)
      | list{')', ..._} as rest if bracketLevel == 0 =>
        let elementsRev = iterStackRev(~stackRev, ~elementsRev)
        next(~elementsRev, rest)
      | list{} as rest =>
        let elementsRev = iterStackRev(~stackRev, ~elementsRev)
        next(~elementsRev, rest)
      | list{'(' as char, ...rest} =>
        iter(
          ~inUnaryPosition=false,
          ~bracketLevel=bracketLevel + 1,
          // Don't put top-level brackets
          ~stackRev=bracketLevel != 0 ? list{char, ...stackRev} : stackRev,
          rest,
        )
      | list{')' as char, ...rest} =>
        iter(
          ~inUnaryPosition=false,
          ~bracketLevel=bracketLevel - 1,
          ~stackRev=bracketLevel != 1 ? list{char, ...stackRev} : stackRev,
          rest,
        )
      | list{char, ...rest} =>
        iter(~inUnaryPosition=false, ~bracketLevel, ~stackRev=list{char, ...stackRev}, rest)
      }

    iter(~inUnaryPosition=true, ~bracketLevel=0, ~stackRev=list{}, input)
  }
  and iterStackRev = (~stackRev, ~elementsRev) => {
    let argInput = Belt.List.reverse(stackRev)
    let elementsRev = iter(~elementsRev, argInput)
    let elementsRev = list{Arg, ...elementsRev}
    elementsRev
  }
)

let ofString = (input: string) => {
  let input = Belt.List.makeBy(String.length(input), (i): char => {
    Obj.magic(StringUtil.charAtUnsafe(input, i))
  })

  let elements = iter(input)->Belt.List.toArray->ArrayUtil.reverseInPlace
  normalize(elements)
}
