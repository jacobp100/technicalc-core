open AST
open Mml_Builders

%%private(let invalidAttributes = list{(#class, "invalid"), (#stretchy, "false")})
module Mml_Accum = Stringifier.Make({
  let groupingSeparatorU = (. locale) => element("mn", Stringifier.groupingSeparator(locale))
  let decimalSeparatorU = (. locale, range) =>
    element(~range, "mn", Stringifier.decimalSeparator(locale))

  let unpairedOpenBracketU = (. range) => element(~attributes=invalidAttributes, ~range, "mo", "(")
  let unpairedCloseBracketU = (. superscript, range) =>
    element(~attributes=invalidAttributes, ~superscript?, ~range, "mo", ")")

  let bracketRangeU = (. superscript, body, openBracketRange, closeBracketRange) =>
    switch superscript {
    | Some({AST.superscriptBody: superscriptBody, index: superscriptIndex}) =>
      // We want the superscript to be over the whole bracket group,
      // not just over the close bracket
      // Every other element works differently to this
      let (closeBracketStart, closeBracketEnd) = closeBracketRange
      let body =
        element(~range=openBracketRange, "mo", "(") ++
        body ++
        element(~range=(closeBracketStart, superscriptIndex), "mo", ")")
      element(
        ~attributes=list{selection(~end=closeBracketEnd, ())},
        "msup",
        element("mrow", body) ++ superscriptBody,
      )
    | None =>
      // Always wrap in mrow so bracket heights don't change when adding superscript
      element(
        "mrow",
        element(~range=openBracketRange, "mo", "(") ++
        body ++
        element(~range=closeBracketRange, "mo", ")"),
      )
    }
})

let map = (. accum, range) => {
  let row = Mml_Accum.toString(. accum)
  if row == "" {
    open Mml_Placeholder
    element(~attributes, ~range, tag, body)
  } else {
    `<mrow>${row}</mrow>`
  }
}

%%private(
  let stringOfBase = base =>
    switch base {
    | Base_Bin => "0b"
    | Base_Oct => "0o"
    | Base_Hex => "0x"
    }
)

%%private(
  let bracketGroup = (leftBracket, rightBracket, arg, superscript, range) => {
    let body = element("mo", leftBracket) ++ arg ++ element("mo", rightBracket)
    element(~superscript?, ~range, "mrow", body)
  }
)

%%private(
  let hat = (~superscript=?, ~range, x) =>
    element(
      ~superscript?,
      ~range,
      "mover",
      element("mi", x) ++ element(~attributes=list{(#stretchy, "false")}, "mo", "^"),
    )
)

%%private(let xSetRow = value => element("mrow", element("mi", "x") ++ element("mo", "=") ++ value))

%%private(
  let functionMml = (~superscript=?, ~range, x) =>
    switch x {
    | Fn_Sin => element(~superscript?, ~range, "mi", "sin")
    | Fn_Asin => element(~superscript?, ~range, "mi", "arcsin")
    | Fn_Cosec => element(~superscript?, ~range, "mi", "cosec")
    | Fn_Sinh => element(~superscript?, ~range, "mi", "sinh")
    | Fn_Asinh => element(~superscript?, ~range, "mi", "arcsinh")
    | Fn_Cos => element(~superscript?, ~range, "mi", "cos")
    | Fn_Acos => element(~superscript?, ~range, "mi", "arccos")
    | Fn_Sec => element(~superscript?, ~range, "mi", "sec")
    | Fn_Cosh => element(~superscript?, ~range, "mi", "cosh")
    | Fn_Acosh => element(~superscript?, ~range, "mi", "arccosh")
    | Fn_Tan => element(~superscript?, ~range, "mi", "tan")
    | Fn_Atan => element(~superscript?, ~range, "mi", "arctan")
    | Fn_Cot => element(~superscript?, ~range, "mi", "cot")
    | Fn_Tanh => element(~superscript?, ~range, "mi", "tanh")
    | Fn_Atanh => element(~superscript?, ~range, "mi", "arctanh")
    | Fn_Deg => element(~superscript?, ~range, "mi", "deg")
    | Fn_Grad => element(~superscript?, ~range, "mi", "grad")
    | Fn_Rad => element(~superscript?, ~range, "mi", "rad")
    | Fn_Log => element(~superscript?, ~range, "mi", "log")
    | Fn_Re => element(~superscript?, ~range, "mi", "re")
    | Fn_Im => element(~superscript?, ~range, "mi", "im")
    | Fn_Rref => element(~superscript?, ~range, "mi", "rref")
    | Fn_Trace => element(~superscript?, ~range, "mi", "tr")
    | Fn_Gamma =>
      element(~superscript?, ~range, ~attributes=list{(#mathvariant, "normal")}, "mi", "&#x0393;")
    | Fn_NLog({base}) =>
      let body = element("mi", "log") ++ base
      element(~superscript?, ~range, "msub", body)
    | Fn_Sum({from, to}) | Fn_Product({from, to}) =>
      let symbol = switch x {
      | Fn_Sum(_) => "&#x2211;"
      | _ => "&#x220F;"
      }
      let body = element("munderover", element("mo", symbol) ++ xSetRow(from) ++ to)
      element(~superscript?, ~range, "mrow", body)
    }
)

%%private(
  let fn2 = (name, a, b, superscript, range) =>
    element(
      ~superscript?,
      ~range,
      "mrow",
      element("mi", name) ++
      element("mo", "(") ++
      a ++
      element("mo", ",") ++
      b ++
      element("mo", ")"),
    )
)

%%private(
  let nprNcr = (symbol, n, r, range) => {
    let nucleus = element(~attributes=list{(#mathvariant, "bold")}, "mi", symbol)
    let body = element("mmultiscripts", `${nucleus}${r}<none /><mprescripts />${n}<none />`)
    element(~range, "mrow", body)
  }
)

%%private(
  let table = (~numRows, ~numColumns, elements, superscript, range) => {
    let inner =
      Belt.Array.makeByU(numRows, (. row) => {
        Belt.Array.makeByU(numColumns, (. column) => {
          element("mtd", elements->Belt.Array.getUnsafe(row * numColumns + column))
        })
        ->StringUtil.join
        ->element("mtr", _)
      })
      ->StringUtil.join
      ->element("mtable", _)
    let body = element("mo", "[") ++ inner ++ element("mo", "]")
    element(~superscript?, ~range, "mrow", body)
  }
)

%%private(
  let supsrscriptSuffix = (~attributes=list{}, ~range, accum, tag, body) =>
    Mml_Accum.modifyLastU(.accum, (. last) => {
      switch last {
      | Some(last) =>
        let (start, end) = range
        element(
          ~attributes=list{selection(~end, ())},
          "msup",
          element("mrow", last) ++
          element(~attributes=list{selection(~avoid=true, ~start, ()), ...attributes}, tag, body),
        )
      | None =>
        element(
          ~range,
          "msup",
          element(
            ~attributes=Mml_Placeholder.attributes,
            Mml_Placeholder.tag,
            Mml_Placeholder.body,
          ) ++
          element(~attributes, tag, body),
        )
      }
    })
)

let reduce = (. accum, stateElement: foldState<string>, range) =>
  switch stateElement {
  | Fold_OpenBracket => Mml_Accum.appendOpenBracket(. accum, range)
  | Fold_CloseBracket(superscript) => Mml_Accum.appendCloseBracket(. accum, range, superscript)
  | Fold_Digit({nucleus, superscript}) =>
    element(~superscript?, ~range, "mn", nucleus)->Mml_Accum.appendDigit(. accum, _)
  | Fold_DecimalSeparator => Mml_Accum.appendDecimalSeparator(. accum, range)
  | Fold_Base(base) =>
    element(~range, "mn", stringOfBase(base))->Mml_Accum.appendBasePrefix(. accum, _)
  | Fold_Percent => element(~range, "mo", "%")->Mml_Accum.append(. accum, _)
  | Fold_Angle(Angle_Degree) => element(~range, "mo", "&#x00B0;")->Mml_Accum.append(. accum, _)
  | Fold_Angle(Angle_ArcMinute) => supsrscriptSuffix(~range, accum, "mo", "&#x2032;")
  | Fold_Angle(Angle_ArcSecond) => supsrscriptSuffix(~range, accum, "mo", "&#x2033;")
  | Fold_Angle(Angle_Radian) =>
    supsrscriptSuffix(~attributes=list{(#mathvariant, "normal")}, ~range, accum, "mi", "r")
  | Fold_Angle(Angle_Gradian) =>
    supsrscriptSuffix(~attributes=list{(#mathvariant, "normal")}, ~range, accum, "mi", "g")
  | Fold_ImaginaryUnit(superscript) =>
    element(~superscript?, ~range, "mi", "i")->Mml_Accum.append(. accum, _)
  | Fold_Conj => supsrscriptSuffix(~range, accum, "mo", "&#x02217;")
  | Fold_Transpose => supsrscriptSuffix(~range, accum, "mi", "T")
  | Fold_Magnitude({value}) =>
    let body = element("mo", "&#x00D7;") ++ element("mn", "10")
    let body = element("mrow", body)
    element(~range, "msup", body ++ value)->Mml_Accum.append(. accum, _)
  | Fold_IterationX(superscript) =>
    element(~superscript?, ~range, "mi", "x")->Mml_Accum.append(. accum, _)
  | Fold_XUnit(superscript) => hat(~superscript?, ~range, "x")->Mml_Accum.append(. accum, _)
  | Fold_YUnit(superscript) => hat(~superscript?, ~range, "y")->Mml_Accum.append(. accum, _)
  | Fold_ZUnit(superscript) => hat(~superscript?, ~range, "z")->Mml_Accum.append(. accum, _)
  | Fold_ConstPi(superscript) =>
    element(~superscript?, ~range, "mi", "&#x03C0;")->Mml_Accum.append(. accum, _)
  | Fold_ConstE(superscript) =>
    element(~superscript?, ~range, "mi", "e")->Mml_Accum.append(. accum, _)
  | Fold_Constant({symbol, superscript})
  | Fold_Variable({symbol, superscript}) =>
    element(~superscript?, ~range, "mrow", Mml_Symbol.toMml(symbol))->Mml_Accum.append(. accum, _)
  | Fold_Placeholder({placeholder, superscript}) =>
    let phantom = element(~attributes=list{selection(~start=fst(range) + 1, ())}, "mphantom", "")
    let symbol = switch placeholder {
    | Some(placeholder) => Mml_Symbol.toMml(placeholder)
    | None =>
      element(
        ~attributes=list{(#mathvariant, Mml_Placeholder.mathvariant)},
        Mml_Placeholder.tag,
        Mml_Placeholder.body,
      )
    }
    element(
      ~attributes=list{(#class, Mml_Placeholder.class)},
      ~superscript?,
      ~range,
      "mrow",
      phantom ++ symbol,
    )->Mml_Accum.append(. accum, _)
  | Fold_Function({fn, resultSuperscript: superscript}) =>
    functionMml(~superscript?, ~range, fn)->Mml_Accum.appendOperatorOrFunction(. accum, _)
  | Fold_Factorial => element(~range, "mo", "!")->Mml_Accum.append(. accum, _)
  | Fold_Add => element(~range, "mo", "+")->Mml_Accum.appendOperatorOrFunction(. accum, _)
  | Fold_Sub => element(~range, "mo", "-")->Mml_Accum.appendOperatorOrFunction(. accum, _)
  | Fold_Mul => element(~range, "mo", "&#x00D7;")->Mml_Accum.appendOperatorOrFunction(. accum, _)
  | Fold_Div => element(~range, "mo", "&#x00F7;")->Mml_Accum.appendOperatorOrFunction(. accum, _)
  | Fold_Dot => element(~range, "mo", "&#xb7;")->Mml_Accum.appendOperatorOrFunction(. accum, _)
  | Fold_Frac({num, den, superscript}) =>
    element(~superscript?, ~range, "mfrac", num ++ den)->Mml_Accum.append(. accum, _)
  | Fold_Sqrt({radicand, superscript}) =>
    element(~superscript?, ~range, "msqrt", radicand)->Mml_Accum.append(. accum, _)
  | Fold_NRoot({degree, radicand, superscript}) =>
    element(~superscript?, ~range, "mroot", radicand ++ degree)->Mml_Accum.append(. accum, _)
  | Fold_Abs({arg, superscript}) =>
    bracketGroup("|", "|", arg, superscript, range)->Mml_Accum.append(. accum, _)
  | Fold_Floor({arg, superscript}) =>
    bracketGroup("&#x230A;", "&#x230B;", arg, superscript, range)->Mml_Accum.append(. accum, _)
  | Fold_Ceil({arg, superscript}) =>
    bracketGroup("&#x2308;", "&#x2309;", arg, superscript, range)->Mml_Accum.append(. accum, _)
  | Fold_Round({arg, superscript}) =>
    bracketGroup("&#x230A;", "&#x2309;", arg, superscript, range)->Mml_Accum.append(. accum, _)
  | Fold_Rand(superscript) =>
    element(~superscript?, ~range, "mi", "rand")->Mml_Accum.append(. accum, _)
  | Fold_RandInt({a, b, superscript}) =>
    let body = element("mi", "rand#") ++ element("mrow", a ++ element("mo", ",") ++ b)
    let body = element("msub", body)
    element(~superscript?, ~range, "mrow", body)->Mml_Accum.append(. accum, _)
  | Fold_Min({a, b, superscript}) =>
    fn2("min", a, b, superscript, range)->Mml_Accum.append(. accum, _)
  | Fold_Max({a, b, superscript}) =>
    fn2("max", a, b, superscript, range)->Mml_Accum.append(. accum, _)
  | Fold_Gcd({a, b, superscript}) =>
    fn2("gcd", a, b, superscript, range)->Mml_Accum.append(. accum, _)
  | Fold_Lcm({a, b, superscript}) =>
    fn2("lcm", a, b, superscript, range)->Mml_Accum.append(. accum, _)
  | Fold_NPR({n, r}) => nprNcr("P", n, r, range)->Mml_Accum.append(. accum, _)
  | Fold_NCR({n, r}) => nprNcr("C", n, r, range)->Mml_Accum.append(. accum, _)
  | Fold_Differential({at, body}) =>
    let pre = element(
      "mfrac",
      element(~attributes=list{(#mathvariant, "normal")}, "mi", "d") ++ element("mi", "dx"),
    )
    let post = element(
      ~attributes=list{(#align, "left")},
      "munder",
      element("mo", "|") ++ xSetRow(at),
    )
    element(~range, "mrow", pre ++ body ++ post)->Mml_Accum.append(. accum, _)
  | Fold_Integral({from, to, body}) =>
    let pre = element("msubsup", element("mo", "&#x222B;") ++ from ++ to)
    let post = element("mi", "dx")
    element(~range, "mrow", pre ++ body ++ post)->Mml_Accum.append(. accum, _)
  | Fold_Table({elements, superscript, numRows, numColumns}) =>
    table(~numRows, ~numColumns, elements, superscript, range)->Mml_Accum.append(. accum, _)
  }

let create = (~locale=Stringifier.English, ~digitGrouping=true, ~inline=false, elements) => {
  let body = if Belt.Array.length(elements) != 0 {
    AST.reduceMapU(elements, ~reduce, ~map, ~initial=Mml_Accum.make(. locale, digitGrouping))
  } else {
    ""
  }
  element(
    "math",
    ~attributes=list{
      (#xmlns, "http://www.w3.org/1998/Math/MathML"),
      (#display, inline ? "inline" : "block"),
    },
    body,
  )
}
