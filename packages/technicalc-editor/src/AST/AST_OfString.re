open AST_Types;

let%private elementExn = x =>
  switch (x) {
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
  | 'A' => NA_S
  | 'b'
  | 'B' => NB_S
  | 'c'
  | 'C' => NC_S
  | 'd'
  | 'D' => ND_S
  | 'e'
  | 'E' => NE_S
  | 'f'
  | 'F' => NF_S
  | _ => assert(false)
  };

let%private rec iter = (~didInsertHex=false, ~elementsRev=[], input) =>
  switch (input) {
  | ['0', 'b', ...rest] =>
    iter(~didInsertHex=false, ~elementsRev=[Bin, ...elementsRev], rest)
  | ['0', 'o', ...rest] =>
    iter(~didInsertHex=false, ~elementsRev=[Oct, ...elementsRev], rest)
  | ['0', 'x', ...rest] =>
    iter(~didInsertHex=true, ~elementsRev=[Hex, ...elementsRev], rest)
  | ['^', ...rest] =>
    let element = Superscript1;
    parseArg(~elementsRev=[element, ...elementsRev], rest);
  | ['*' | 'x', '1', '0', '^', ...rest]
  | ['*' | 'x', ' ', '1', '0', '^', ...rest]
  | ['*' | 'x', '1', '0', ' ', '^', ...rest]
  | ['*' | 'x', ' ', '1', '0', ' ', '^', ...rest] =>
    let element = Magnitude1;
    parseArg(~elementsRev=[element, ...elementsRev], rest);
  | ['e' | 'E', ...rest] when !didInsertHex =>
    let element = Magnitude1;
    parseArg(~elementsRev=[element, ...elementsRev], rest);
  | ['a'..'f' as digit, ...rest]
  | ['A'..'F' as digit, ...rest] when didInsertHex =>
    let element = elementExn(digit);
    iter(~didInsertHex, ~elementsRev=[element, ...elementsRev], rest);
  | [('0'..'9' | '.') as digit, ...rest] =>
    let element = elementExn(digit);
    iter(~didInsertHex, ~elementsRev=[element, ...elementsRev], rest);
  | [('+' | '-' | '*' | '/' | '!' | '%' | '(' | ')') as operatorLike, ...rest] =>
    let element = elementExn(operatorLike);
    iter(~didInsertHex=false, ~elementsRev=[element, ...elementsRev], rest);
  | [_, ...rest] => iter(~didInsertHex, ~elementsRev, rest)
  | [] => elementsRev
  }
and parseArg = (~elementsRev, input) =>
  switch (input) {
  | ['(', ...rest] => parseUntilCloseBracket(~elementsRev, rest)
  | _ => parseUntilOperator(~elementsRev, input)
  }
and parseUntilCloseBracket =
    (~stackRev=[], ~bracketLevel=1, ~elementsRev, input) => {
  let bracketLevel =
    switch (input) {
    | [')', ..._] => bracketLevel - 1
    | ['(', ..._] => bracketLevel + 1
    | _ => bracketLevel
    };
  switch (input) {
  | [_ /* close bracket */, ...rest] when bracketLevel == 0 =>
    let elementsRev = iterStackRev(~stackRev, ~elementsRev);
    iter(~elementsRev, rest);
  | [] =>
    let elementsRev = iterStackRev(~stackRev, ~elementsRev);
    elementsRev;
  | [char, ...rest] =>
    parseUntilCloseBracket(
      ~stackRev=[char, ...stackRev],
      ~bracketLevel,
      ~elementsRev,
      rest,
    )
  };
}
and parseUntilOperator =
    (~inUnaryPosition=true, ~stackRev=[], ~elementsRev, input) =>
  switch (input) {
  | [('+' | '-' | '*' | '/') as char, ...rest]
      when !(inUnaryPosition && (char == '+' || char == '-')) =>
    let elementsRev = iterStackRev(~stackRev, ~elementsRev);
    let elementsRev = [elementExn(char), ...elementsRev];
    iter(~elementsRev, rest);
  | [] =>
    let elementsRev = iterStackRev(~stackRev, ~elementsRev);
    elementsRev;
  | [char, ...rest] =>
    parseUntilOperator(
      ~inUnaryPosition=false,
      ~stackRev=[char, ...stackRev],
      ~elementsRev,
      rest,
    )
  }
and iterStackRev = (~stackRev, ~elementsRev) => {
  let argInput = Belt.List.reverse(stackRev);
  let elementsRev = iter(~elementsRev, argInput);
  let elementsRev = [Arg, ...elementsRev];
  elementsRev;
};

let ofString = (input: string) => {
  let input =
    Belt.List.makeByU(String.length(input), (. i) => {
      (Obj.magic(StringUtil.charAtUnsafe(input, i)): char)
    });

  let elements = iter(input)->Belt.List.toArray->ArrayUtil.reverseInPlace;
  normalize(elements);
};
