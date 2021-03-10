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
    | 'a'
    | 'A' =>
      NA_S
    | 'b'
    | 'B' =>
      NB_S
    | 'c'
    | 'C' =>
      NC_S
    | 'd'
    | 'D' =>
      ND_S
    | 'e'
    | 'E' =>
      NE_S
    | 'f'
    | 'F' =>
      NF_S
    | _ => assert false
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
  and parseArg = (~elementsRev, input) =>
    switch input {
    | list{'(', ...rest} => parseUntilCloseBracket(~elementsRev, rest)
    | _ => parseUntilOperator(~elementsRev, input)
    }
  and parseUntilCloseBracket = (~stackRev=list{}, ~bracketLevel=1, ~elementsRev, input) => {
    let bracketLevel = switch input {
    | list{')', ..._} => bracketLevel - 1
    | list{'(', ..._} => bracketLevel + 1
    | _ => bracketLevel
    }
    switch input {
    | list{_, ...rest} if bracketLevel == 0 =>
      let elementsRev = iterStackRev(~stackRev, ~elementsRev)
      iter(~elementsRev, rest)
    | list{} =>
      let elementsRev = iterStackRev(~stackRev, ~elementsRev)
      elementsRev
    | list{char, ...rest} =>
      parseUntilCloseBracket(~stackRev=list{char, ...stackRev}, ~bracketLevel, ~elementsRev, rest)
    }
  }
  and parseUntilOperator = (~inUnaryPosition=true, ~stackRev=list{}, ~elementsRev, input) =>
    switch input {
    | list{('+' | '-' | '*' | '/') as char, ...rest}
      if !(inUnaryPosition && (char == '+' || char == '-')) =>
      let elementsRev = iterStackRev(~stackRev, ~elementsRev)
      let elementsRev = list{elementExn(char), ...elementsRev}
      iter(~elementsRev, rest)
    | list{} =>
      let elementsRev = iterStackRev(~stackRev, ~elementsRev)
      elementsRev
    | list{char, ...rest} =>
      parseUntilOperator(
        ~inUnaryPosition=false,
        ~stackRev=list{char, ...stackRev},
        ~elementsRev,
        rest,
      )
    }
  and iterStackRev = (~stackRev, ~elementsRev) => {
    let argInput = Belt.List.reverse(stackRev)
    let elementsRev = iter(~elementsRev, argInput)
    let elementsRev = list{Arg, ...elementsRev}
    elementsRev
  }
)

let ofString = (input: string) => {
  let input = Belt.List.makeByU(String.length(input), (. i): char =>
    Obj.magic(StringUtil.charAtUnsafe(input, i))
  )

  let elements = iter(input)->Belt.List.toArray->ArrayUtil.reverseInPlace
  normalize(elements)
}
