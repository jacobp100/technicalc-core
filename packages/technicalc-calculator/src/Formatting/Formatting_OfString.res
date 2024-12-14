type token =
  | @as(0) Integer(string)
  | @as(1) NaN
  | @as(2) Base2
  | @as(3) Base8
  | @as(4) Base16
  | @as(5) Sqrt
  | @as(6) Exp
  | @as(7) Pi
  | @as(8) E
  | @as(9) I
  | @as(10) Dot
  | @as(11) Plus
  | @as(12) Minus
  | @as(13) Slash
  | @as(14) Percent
  | @as(15) OpenBracket
  | @as(16) CloseBracket
  | @as(17) OpenBrace
  | @as(18) CloseBrace
  | @as(19) Comma

%%private(
  let rec iter = (~tokensRev, ~parseExponent, charList) =>
    switch charList {
    | list{'N', 'a', 'N', ...rest}
    | list{'n', 'a', 'n', ...rest} =>
      append(~tokensRev, ~parseExponent, rest, NaN)
    | list{'0', 'b', ...rest} => append(~tokensRev, ~parseExponent, rest, Base2)
    | list{'0', 'o', ...rest} => append(~tokensRev, ~parseExponent, rest, Base8)
    | list{'0', 'x', ...rest} => append(~tokensRev, ~parseExponent, rest, Base16)
    | list{'s', 'q', 'r', 't', ...rest} => append(~tokensRev, ~parseExponent, rest, Sqrt)
    | list{'e', 'x', 'p', ...rest} => append(~tokensRev, ~parseExponent, rest, Exp)
    | list{'p', 'i', ...rest} => append(~tokensRev, ~parseExponent, rest, Pi)
    | list{'E' | 'e', ...rest} if parseExponent => append(~tokensRev, ~parseExponent, rest, E)
    | list{'i', ...rest} => append(~tokensRev, ~parseExponent, rest, I)
    | list{'.', ...rest} => append(~tokensRev, ~parseExponent, rest, Dot)
    | list{'+', ...rest} => append(~tokensRev, ~parseExponent, rest, Plus)
    | list{'-', ...rest} => append(~tokensRev, ~parseExponent, rest, Minus)
    | list{'/', ...rest} => append(~tokensRev, ~parseExponent, rest, Slash)
    | list{'%', ...rest} => append(~tokensRev, ~parseExponent, rest, Percent)
    | list{'(', ...rest} => append(~tokensRev, ~parseExponent, rest, OpenBracket)
    | list{')', ...rest} => append(~tokensRev, ~parseExponent, rest, CloseBracket)
    | list{'{', ...rest} => append(~tokensRev, ~parseExponent, rest, OpenBrace)
    | list{'}', ...rest} => append(~tokensRev, ~parseExponent, rest, CloseBrace)
    | list{',', ...rest} => append(~tokensRev, ~parseExponent, rest, Comma)
    | list{('0' .. '9' | 'a' .. 'f' | 'A' .. 'F') as char, ...rest} =>
      let char = Obj.magic(char)->StringUtil.ofChar
      let tokensRev = switch tokensRev {
      | list{Integer(existing), ...rest} => list{Integer(existing ++ char), ...rest}
      | _ => list{Integer(char), ...tokensRev}
      }
      iter(~tokensRev, ~parseExponent, rest)
    | list{' ', ...rest} => iter(~tokensRev, ~parseExponent, rest)
    | list{} => Some(Belt.List.reverse(tokensRev))
    | _ => None
    }
  and append = (~tokensRev, ~parseExponent, charList, token) =>
    iter(~tokensRev=list{token, ...tokensRev}, ~parseExponent, charList)
)
%%private(
  let tokenizeBase = (~base, string) => {
    let parseExponent = switch base {
    | Some(10)
    | None => true
    | _ => false
    }
    iter(
      ~tokensRev=list{},
      ~parseExponent,
      Belt.List.makeBy(String.length(string), i => Obj.magic(StringUtil.charAtUnsafe(string, i))),
    )
  }
)

%%private(
  let partialParseConstant = (~base, tokens) =>
    switch tokens {
    | _ if base != None => None
    | list{Sqrt, OpenBracket, Integer(sqrt), CloseBracket, ...rest} =>
      switch Belt.Int.fromString(sqrt) {
      | Some(sqrt) => Some((Real_Constant.Sqrt(sqrt), rest))
      | None => None
      }
    | list{Exp, OpenBracket, Integer(exp), CloseBracket, ...rest} =>
      switch Belt.Int.fromString(exp) {
      | Some(exp) => Some((Real_Constant.Exp(exp), rest))
      | None => None
      }
    | list{Pi, ...rest} => Some((Pi(1), rest))
    | rest => Some((Unit, rest))
    }
)
%%private(
  let partialParseFraction = (~base, tokens) => {
    let tokens = base != None ? None : Some(tokens)
    let (num, constant, tokens) = switch tokens {
    | Some(list{Integer(num), ...rest}) =>
      let num = Decimal.ofString(num)
      switch partialParseConstant(~base, rest) {
      | Some((constant, rest)) => (num, Some(constant), Some(rest))
      | None => (num, None, Some(rest))
      }
    | Some(rest) =>
      switch partialParseConstant(~base, rest) {
      | Some((constant, rest)) => (Decimal.one, Some(constant), Some(rest))
      | None => (Decimal.nan, None, None)
      }
    | None => (Decimal.nan, None, None)
    }
    let (den, constant, tokens) = switch tokens {
    | Some(list{Slash, Integer(den), ...rest}) =>
      let den = Decimal.ofString(den)
      switch constant {
      | Some(constant) => (den, Some(constant), Some(rest))
      | None =>
        switch partialParseConstant(~base, rest) {
        | Some((constant, rest)) => (num, Some(constant), Some(rest))
        | None => (num, None, Some(rest))
        }
      }
    | _ => (Decimal.nan, None, None)
    }
    let constant = Belt.Option.getWithDefault(constant, Unit)
    switch tokens {
    | Some(tokens) => Some((num, den, constant, tokens))
    | _ => None
    }
  }
)
%%private(
  let parseDecimal = (~base, tokens) => {
    let (parsedBase, tokens) = switch (base, tokens) {
    | (Some(base), _) => (base, tokens)
    | (None, list{Base2, ...rest}) => (2, rest)
    | (None, list{Base8, ...rest}) => (8, rest)
    | (None, list{Base16, ...rest}) => (16, rest)
    | (_, rest) => (10, rest)
    }
    let (integerStr, decimalAscii, tokens) = switch tokens {
    | list{Integer(int), Dot, Integer(dec), ...rest} => (int, Some(dec), Some(rest))
    | list{Integer(int), Dot, ...rest} => (int, None, Some(rest))
    | list{Integer(int), ...rest} => (int, None, Some(rest))
    | list{Dot, Integer(dec), ...rest} => ("0", Some(dec), Some(rest))
    | _ => ("", None, None)
    }
    let (expMagnitudeAscii, tokens) = switch tokens {
    | Some(list{E, Integer(int), ...rest})
    | Some(list{E, Plus, Integer(int), ...rest}) => (Some(int), Some(rest))
    | Some(list{E, Minus, Integer(int), ...rest}) => (Some("-" ++ int), Some(rest))
    | Some(_) as tokens => (None, tokens)
    | None => (None, None)
    }
    let (num, den) = if tokens != None {
      open Decimal
      let (basePrefix, base) = switch parsedBase {
      | 10 => ("", 10)
      | 2 => ("0b", 2)
      | 8 => ("0o", 8)
      | 16 => ("0x", 16)
      | _ => assert(false)
      }
      let (num, den) = switch decimalAscii {
      | Some(decimalAscii) => (
          ofString(basePrefix ++ integerStr ++ decimalAscii),
          ofInt(base) ** ofInt(String.length(decimalAscii)),
        )
      | None => (ofString(basePrefix ++ integerStr), one)
      }
      let expMagnitude = Belt.Option.mapWithDefault(expMagnitudeAscii, zero, ofString)
      switch cmp(expMagnitude, zero) {
      | 1 => (num * ofInt(base) ** expMagnitude, den)
      | -1 => (num, den * ofInt(base) ** abs(expMagnitude))
      | _ => (num, den)
      }
    } else {
      (Decimal.nan, Decimal.nan)
    }
    switch tokens {
    | Some(rest) =>
      switch partialParseConstant(~base, rest) {
      | Some((constant, rest)) => Some((num, den, constant, rest))
      | None => Some((num, den, Unit, rest))
      }
    | None => None
    }
  }
)
%%private(
  let partialParseReal = (~base, tokens) => {
    let state = partialParseFraction(~base, tokens)
    let state = state == None ? parseDecimal(~base, tokens) : state
    let state = switch state == None ? partialParseConstant(~base, tokens) : None {
    | Some((constant, rest)) => Some((Decimal.one, Decimal.one, constant, rest))
    | None => state
    }
    switch state {
    | Some((num, den, constant, rest)) =>
      let value = switch (
        Decimal.toFloat(num)->FloatUtil.toInt,
        Decimal.toFloat(den)->FloatUtil.toInt,
      ) {
      | (Some(num), Some(den)) => Real.ofRational(num, den, constant)
      | _ =>
        Real.ofDecimal({
          open Decimal
          num / den * Real_Constant.toDecimal(constant)
        })
      }
      Some((value, rest))
    | None => None
    }
  }
)

%%private(
  let partialParseScalar = (~base, tokens) => {
    let (positive, tokens) = switch tokens {
    | list{Minus, ...rest} => (false, rest)
    | list{Plus, ...rest} => (true, rest)
    | tokens => (true, tokens)
    }
    let (real, tokens) = switch (positive, partialParseReal(~base, tokens)) {
    | (true, Some((value, tokens))) => (value, Some(tokens))
    | (false, Some((value, tokens))) => (Real.neg(value), Some(tokens))
    | (_, None) => (Real.nan, None)
    }
    switch tokens {
    | Some(list{(Plus | Minus) as sign, ...rest}) =>
      switch partialParseReal(~base, rest) {
      | Some((imag, list{I, ...rest})) =>
        let imag = sign == Plus ? imag : Real.neg(imag)
        Some((Scalar.ofComplex(real, imag), rest))
      | _ => None
      }
    | Some(list{I, ...rest}) => Some((Scalar.ofImag(real), rest))
    | Some(rest) => Some((Scalar.ofReal(real), rest))
    | None => None
    }
  }
)

type rec astConstructor =
  | @as(0) Scalar(Scalar.t)
  | @as(1) Percent(Scalar.t)
  | @as(2) Row(array<astConstructor>)

%%private(
  let parseValue = (~base, tokens) => {
    let rec iter = tokens =>
      switch tokens {
      | list{OpenBrace, ...rest} => parseUntilCloseBraceDefault(rest)
      | rest =>
        switch partialParseScalar(~base, rest) {
        | Some((scalar, list{})) => Some((Scalar(scalar), list{}))
        | Some((scalar, list{Percent})) => Some((Percent(scalar), list{}))
        | _ => None
        }
      }
    // FIXME - there's a bug in ReScript stopping you calling parseUntilCloseBrace(tokens)
    and parseUntilCloseBraceDefault = tokens =>
      parseUntilCloseBrace(~tokensAccumRev=list{}, ~elementsRev=list{}, ~level=0, tokens)
    and parseUntilCloseBrace = (~tokensAccumRev, ~elementsRev, ~level, tokens) =>
      switch tokens {
      | list{CloseBrace, ...rest} if level == 0 =>
        let elementsRev = switch iter(Belt.List.reverse(tokensAccumRev)) {
        | Some((closedElement, list{})) => Some(list{closedElement, ...elementsRev})
        | _ => None
        }
        switch elementsRev {
        | Some(list{scalar}) => Some((scalar, rest))
        | Some(elementsRev) =>
          let elements = Belt.List.toArray(elementsRev)
          Belt.Array.reverseInPlace(elements)
          Some((Row(elements), rest))
        | None => None
        }
      | list{Comma, ...rest} if level == 0 =>
        switch iter(Belt.List.reverse(tokensAccumRev)) {
        | Some((element, list{})) =>
          parseUntilCloseBrace(
            ~tokensAccumRev=list{},
            ~elementsRev=list{element, ...elementsRev},
            ~level,
            rest,
          )
        | _ => None
        }
      | list{e, ...rest} =>
        let level = switch e {
        | CloseBrace => level - 1
        | OpenBrace => level + 1
        | _ => level
        }
        parseUntilCloseBrace(~tokensAccumRev=list{e, ...tokensAccumRev}, ~elementsRev, ~level, rest)
      | list{} => None
      }

    let ast = switch iter(tokens) {
    | Some((ast, list{})) => Some(ast)
    | _ => None
    }

    switch ast {
    | Some(Scalar(scalar)) => Some(Value_Base.ofScalar(scalar))
    | Some(Row([Scalar(a), Scalar(b)]))
    | Some(Row([Row([Scalar(a)]), Row([Scalar(b)])])) =>
      Some(Vector.make([a, b])->Value_Base.ofVector)
    | Some(Row([Scalar(a), Scalar(b), Scalar(c)]))
    | Some(Row([Row([Scalar(a)]), Row([Scalar(b)]), Row([Scalar(c)])])) =>
      Some(Vector.make([a, b, c])->Value_Base.ofVector)
    | Some(Row([Row([Scalar(a), Scalar(b)]), Row([Scalar(c), Scalar(d)])])) =>
      Some(Matrix.make(~numRows=2, ~numColumns=2, [a, b, c, d])->Value_Base.ofMatrix)
    | Some(Row([
        Row([Scalar(a), Scalar(b), Scalar(c)]),
        Row([Scalar(d), Scalar(e), Scalar(f)]),
        Row([Scalar(g), Scalar(h), Scalar(i)]),
      ])) =>
      Some(Matrix.make(~numRows=3, ~numColumns=3, [a, b, c, d, e, f, g, h, i])->Value_Base.ofMatrix)
    | _ => None
    }
  }
)

%%private(
  let parse = (~base, string) =>
    switch tokenizeBase(~base, string) {
    | Some(list{NaN}) => Some(Value_Base.nan)
    | Some(tokens) => parseValue(~base, tokens)
    | None => None
    }
)

let ofStringBase = (base, string) => parse(~base=Some(base), string)
let ofString = string => parse(~base=None, string)
