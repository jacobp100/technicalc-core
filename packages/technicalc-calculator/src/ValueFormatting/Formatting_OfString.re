type token =
  | Integer(string)
  | NaN
  | Base2
  | Base8
  | Base16
  | Sqrt
  | Exp
  | Pi
  | E
  | I
  | Dot
  | Plus
  | Minus
  | Slash
  | Percent
  | OpenBracket
  | CloseBracket
  | OpenBrace
  | CloseBrace
  | Comma;

let%private rec iter = (~tokensRev, ~parseExponent, charList) =>
  switch (charList) {
  | ['N', 'a', 'N', ...rest]
  | ['n', 'a', 'n', ...rest] => append(~tokensRev, ~parseExponent, rest, NaN)
  | ['0', 'b', ...rest] => append(~tokensRev, ~parseExponent, rest, Base2)
  | ['0', 'o', ...rest] => append(~tokensRev, ~parseExponent, rest, Base8)
  | ['0', 'x', ...rest] => append(~tokensRev, ~parseExponent, rest, Base16)
  | ['s', 'q', 'r', 't', ...rest] =>
    append(~tokensRev, ~parseExponent, rest, Sqrt)
  | ['e', 'x', 'p', ...rest] => append(~tokensRev, ~parseExponent, rest, Exp)
  | ['p', 'i', ...rest] => append(~tokensRev, ~parseExponent, rest, Pi)
  | ['E' | 'e', ...rest] when parseExponent =>
    append(~tokensRev, ~parseExponent, rest, E)
  | ['i', ...rest] => append(~tokensRev, ~parseExponent, rest, I)
  | ['.', ...rest] => append(~tokensRev, ~parseExponent, rest, Dot)
  | ['+', ...rest] => append(~tokensRev, ~parseExponent, rest, Plus)
  | ['-', ...rest] => append(~tokensRev, ~parseExponent, rest, Minus)
  | ['/', ...rest] => append(~tokensRev, ~parseExponent, rest, Slash)
  | ['%', ...rest] => append(~tokensRev, ~parseExponent, rest, Percent)
  | ['(', ...rest] => append(~tokensRev, ~parseExponent, rest, OpenBracket)
  | [')', ...rest] => append(~tokensRev, ~parseExponent, rest, CloseBracket)
  | ['{', ...rest] => append(~tokensRev, ~parseExponent, rest, OpenBrace)
  | ['}', ...rest] => append(~tokensRev, ~parseExponent, rest, CloseBrace)
  | [',', ...rest] => append(~tokensRev, ~parseExponent, rest, Comma)
  | [('0'..'9' | 'a'..'f' | 'A'..'F') as char, ...rest] =>
    let char = String.make(1, char);
    let tokensRev =
      switch (tokensRev) {
      | [Integer(existing), ...rest] => [
          Integer(existing ++ char),
          ...rest,
        ]
      | _ => [Integer(char), ...tokensRev]
      };
    iter(~tokensRev, ~parseExponent, rest);
  | [' ', ...rest] => iter(~tokensRev, ~parseExponent, rest)
  | [] => Some(Belt.List.reverse(tokensRev))
  | _ => None
  }
and append = (~tokensRev, ~parseExponent, charList, token) => {
  iter(~tokensRev=[token, ...tokensRev], ~parseExponent, charList);
};
let%private tokenizeBase = (~base, string) => {
  let parseExponent =
    switch (base) {
    | Some(10)
    | None => true
    | _ => false
    };
  iter(
    ~tokensRev=[],
    ~parseExponent,
    Belt.List.makeBy(String.length(string), i => {
      Obj.magic(StringUtil.charAtUnsafe(string, i))
    }),
  );
};

let%private partialParseConstant = (~base, tokens) =>
  switch (tokens) {
  | _ when base != None => None
  | [Sqrt, OpenBracket, Integer(sqrt), CloseBracket, ...rest] =>
    switch (Belt.Int.fromString(sqrt)) {
    | Some(sqrt) => Some((Real_Constant.Sqrt(sqrt), rest))
    | None => None
    }
  | [Exp, OpenBracket, Integer(exp), CloseBracket, ...rest] =>
    switch (Belt.Int.fromString(exp)) {
    | Some(exp) => Some((Real_Constant.Exp(exp), rest))
    | None => None
    }
  | [Pi, ...rest] => Some((Pi, rest))
  | rest => Some((Unit, rest))
  };
let%private partialParseFraction = (~base, tokens) => {
  let tokens = base != None ? None : Some(tokens);
  let (num, constant, tokens) =
    switch (tokens) {
    | Some([Integer(num), ...rest]) =>
      let num = Decimal.ofString(num);
      switch (partialParseConstant(~base, rest)) {
      | Some((constant, rest)) => (num, Some(constant), Some(rest))
      | None => (num, None, Some(rest))
      };
    | Some(rest) =>
      switch (partialParseConstant(~base, rest)) {
      | Some((constant, rest)) => (Decimal.one, Some(constant), Some(rest))
      | None => (Decimal.nan, None, None)
      }
    | None => (Decimal.nan, None, None)
    };
  let (den, constant, tokens) =
    switch (tokens) {
    | Some([Slash, Integer(den), ...rest]) =>
      let den = Decimal.ofString(den);
      switch (constant) {
      | Some(constant) => (den, Some(constant), Some(rest))
      | None =>
        switch (partialParseConstant(~base, rest)) {
        | Some((constant, rest)) => (num, Some(constant), Some(rest))
        | None => (num, None, Some(rest))
        }
      };
    | _ => (Decimal.nan, None, None)
    };
  let constant = Belt.Option.getWithDefault(constant, Unit);
  switch (tokens) {
  | Some(tokens) => Some((num, den, constant, tokens))
  | _ => None
  };
};
let%private parseDecimal = (~base, tokens) => {
  let (parsedBase, tokens) =
    switch (base, tokens) {
    | (Some(base), _) => (base, tokens)
    | (None, [Base2, ...rest]) => (2, rest)
    | (None, [Base8, ...rest]) => (8, rest)
    | (None, [Base16, ...rest]) => (16, rest)
    | (_, rest) => (10, rest)
    };
  let (integerStr, decimalString, tokens) =
    switch (tokens) {
    | [Integer(int), Dot, Integer(dec), ...rest] => (
        int,
        Some(dec),
        Some(rest),
      )
    | [Integer(int), Dot, ...rest] => (int, None, Some(rest))
    | [Integer(int), ...rest] => (int, None, Some(rest))
    | [Dot, Integer(dec), ...rest] => ("0", Some(dec), Some(rest))
    | _ => ("", None, None)
    };
  let (exp10MagnitudeString, tokens) =
    switch (tokens) {
    | Some([E, Integer(int), ...rest])
    | Some([E, Plus, Integer(int), ...rest]) => (Some(int), Some(rest))
    | Some([E, Minus, Integer(int), ...rest]) => (
        Some("-" ++ int),
        Some(rest),
      )
    | Some(_) as tokens => (None, tokens)
    | None => (None, None)
    };
  let (num, den) =
    if (tokens != None) {
      let (basePrefix, base) =
        switch (parsedBase) {
        | 10 => ("", 10)
        | 2 => ("0b", 2)
        | 8 => ("0o", 8)
        | 16 => ("0x", 16)
        | _ => assert(false)
        };
      let (num, den) =
        switch (decimalString) {
        | Some(decimalString) => (
            Decimal.(ofString(basePrefix ++ integerStr ++ decimalString)),
            Decimal.(ofInt(base) ** ofInt(String.length(decimalString))),
          )
        | None => (Decimal.ofString(basePrefix ++ integerStr), Decimal.one)
        };
      let exp10Magnitude =
        Belt.Option.mapWithDefault(
          exp10MagnitudeString,
          Decimal.zero,
          Decimal.ofString,
        );
      switch (Decimal.(cmp(exp10Magnitude, zero))) {
      | 1 => (Decimal.(num * ofInt(10) ** exp10Magnitude), den)
      | (-1) => (num, Decimal.(den * ofInt(10) ** abs(exp10Magnitude)))
      | _ => (num, den)
      };
    } else {
      (Decimal.nan, Decimal.nan);
    };
  switch (tokens) {
  | Some(rest) =>
    switch (partialParseConstant(~base, rest)) {
    | Some((constant, rest)) => Some((num, den, constant, rest))
    | None => Some((num, den, Unit, rest))
    }
  | None => None
  };
};
let%private partialParseReal = (~base, tokens) => {
  let state = partialParseFraction(~base, tokens);
  let state = state == None ? parseDecimal(~base, tokens) : state;
  let state =
    switch (state == None ? partialParseConstant(~base, tokens) : None) {
    | Some((constant, rest)) =>
      Some((Decimal.one, Decimal.one, constant, rest))
    | None => state
    };
  switch (state) {
  | Some((num, den, constant, rest)) =>
    let value =
      switch (
        Decimal.toFloat(num)->FloatUtil.intValue,
        Decimal.toFloat(den)->FloatUtil.intValue,
      ) {
      | (Some(num), Some(den)) => Real.ofRational(num, den, constant)
      | _ =>
        Real.ofDecimal(
          Decimal.(num / den * Real_Constant.toDecimal(constant)),
        )
      };
    Some((value, rest));
  | None => None
  };
};

let%private partialParseScalar = (~base, tokens) => {
  let (positive, tokens) =
    switch (tokens) {
    | [Minus, ...rest] => (false, rest)
    | [Plus, ...rest] => (true, rest)
    | tokens => (true, tokens)
    };
  let (real, tokens) =
    switch (positive, partialParseReal(~base, tokens)) {
    | (true, Some((value, tokens))) => (value, Some(tokens))
    | (false, Some((value, tokens))) => (Real.neg(value), Some(tokens))
    | (_, None) => (Real.nan, None)
    };
  switch (tokens) {
  | Some([(Plus | Minus) as sign, ...rest]) =>
    switch (partialParseReal(~base, rest)) {
    | Some((imag, [I, ...rest])) =>
      let imag = sign == Plus ? imag : Real.neg(imag);
      Some((`C((real, imag)), rest));
    | _ => None
    }
  | Some([I, ...rest]) => Some((`I(real), rest))
  | Some(rest) => Some((`R(real), rest))
  | None => None
  };
};

let%private parseValue = (~base, tokens) => {
  let rec iter = tokens =>
    switch (tokens) {
    | [OpenBrace, ...rest] => parseUntilCloseBrace(rest)
    | rest =>
      switch (partialParseScalar(~base, rest)) {
      | Some((scalar, [])) => Some((`Scalar(scalar), []))
      | Some((scalar, [Percent])) => Some((`Percent(scalar), []))
      | _ => None
      }
    }
  and parseUntilCloseBrace =
      (~tokensAccumRev=[], ~elementsRev=[], ~level=0, tokens) => {
    switch (tokens) {
    | [CloseBrace, ...rest] when level == 0 =>
      let elementsRev =
        switch (iter(Belt.List.reverse(tokensAccumRev))) {
        | Some((closedElement, [])) => Some([closedElement, ...elementsRev])
        | _ => None
        };
      switch (elementsRev) {
      | Some([scalar]) => Some((scalar, rest))
      | Some(elementsRev) =>
        Some((`Row(Belt.List.reverse(elementsRev)), rest))
      | None => None
      };
    | [Comma, ...rest] when level == 0 =>
      switch (iter(Belt.List.reverse(tokensAccumRev))) {
      | Some((element, [])) =>
        parseUntilCloseBrace(
          ~tokensAccumRev=[],
          ~elementsRev=[element, ...elementsRev],
          ~level,
          rest,
        )
      | _ => None
      }
    | [e, ...rest] =>
      let level =
        switch (e) {
        | CloseBrace => level - 1
        | OpenBrace => level + 1
        | _ => level
        };
      parseUntilCloseBrace(
        ~tokensAccumRev=[e, ...tokensAccumRev],
        ~elementsRev,
        ~level,
        rest,
      );
    | [] => None
    };
  };

  let ast =
    switch (iter(tokens)) {
    | Some((ast, [])) => Some(ast)
    | _ => None
    };

  switch (ast) {
  | Some(`Scalar(scalar)) => Some(Value_Base.ofScalar(scalar))
  | Some(`Row([`Scalar(a), `Scalar(b)]))
  | Some(`Row([`Row([`Scalar(a)]), `Row([`Scalar(b)])])) =>
    Some(Value_Base.ofVector([|a, b|]))
  | Some(`Row([`Scalar(a), `Scalar(b), `Scalar(c)]))
  | Some(
      `Row([`Row([`Scalar(a)]), `Row([`Scalar(b)]), `Row([`Scalar(c)])]),
    ) =>
    Some(Value_Base.ofVector([|a, b, c|]))
  | Some(
      `Row([
        `Row([`Scalar(a), `Scalar(b)]),
        `Row([`Scalar(c), `Scalar(d)]),
      ]),
    ) =>
    Matrix.make(2, 2, [|a, b, c, d|])->Value_Base.ofMatrix->Some
  | Some(
      `Row([
        `Row([`Scalar(a), `Scalar(b), `Scalar(c)]),
        `Row([`Scalar(d), `Scalar(e), `Scalar(f)]),
        `Row([`Scalar(g), `Scalar(h), `Scalar(i)]),
      ]),
    ) =>
    Matrix.make(3, 3, [|a, b, c, d, e, f, g, h, i|])
    ->Value_Base.ofMatrix
    ->Some
  | _ => None
  };
};

let%private parse = (~base, string) =>
  switch (tokenizeBase(~base, string)) {
  | Some([NaN]) => Some(Value_Base.nan)
  | Some(tokens) => parseValue(~base, tokens)
  | None => None
  };

let ofStringBase = (base, string) => parse(~base=Some(base), string);
let ofString = string => parse(~base=None, string);
