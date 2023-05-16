open AST
open Mml_Builders

let map = (. accum, isPlaceholder) => {
  let attributes = isPlaceholder ? list{(#class, "placeholder")} : list{}
  Mml_Accum.toString(~attributes, accum)
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
  let appendBracketGroup = (accum, leftBracket, rightBracket, arg, superscript, range) => {
    let body = mml("mo", leftBracket) ++ arg ++ mml("mo", rightBracket)
    Mml_Accum.append(accum, ~superscript?, ~range, "mrow", body)
  }
)

%%private(
  let appendHat = (accum, ~superscript=?, ~range, x) =>
    Mml_Accum.append(
      accum,
      ~superscript?,
      ~range,
      "mover",
      mml("mi", x) ++ mml(~attributes=list{(#stretchy, "false")}, "mo", "^"),
    )
)

%%private(let xSetRow = value => mml("mrow", mml("mi", "x") ++ mml("mo", "=") ++ value))

%%private(
  let appendFunctionMml = (accum, ~superscript=?, ~range, x) =>
    switch x {
    | Fn_Sin => Mml_Accum.appendOperatorOrFunction(accum, ~superscript?, ~range, "mi", "sin")
    | Fn_Asin => Mml_Accum.appendOperatorOrFunction(accum, ~superscript?, ~range, "mi", "arcsin")
    | Fn_Cosec => Mml_Accum.appendOperatorOrFunction(accum, ~superscript?, ~range, "mi", "cosec")
    | Fn_Sinh => Mml_Accum.appendOperatorOrFunction(accum, ~superscript?, ~range, "mi", "sinh")
    | Fn_Asinh => Mml_Accum.appendOperatorOrFunction(accum, ~superscript?, ~range, "mi", "arcsinh")
    | Fn_Cos => Mml_Accum.appendOperatorOrFunction(accum, ~superscript?, ~range, "mi", "cos")
    | Fn_Acos => Mml_Accum.appendOperatorOrFunction(accum, ~superscript?, ~range, "mi", "arccos")
    | Fn_Sec => Mml_Accum.appendOperatorOrFunction(accum, ~superscript?, ~range, "mi", "sec")
    | Fn_Cosh => Mml_Accum.appendOperatorOrFunction(accum, ~superscript?, ~range, "mi", "cosh")
    | Fn_Acosh => Mml_Accum.appendOperatorOrFunction(accum, ~superscript?, ~range, "mi", "arccosh")
    | Fn_Tan => Mml_Accum.appendOperatorOrFunction(accum, ~superscript?, ~range, "mi", "tan")
    | Fn_Atan => Mml_Accum.appendOperatorOrFunction(accum, ~superscript?, ~range, "mi", "arctan")
    | Fn_Cot => Mml_Accum.appendOperatorOrFunction(accum, ~superscript?, ~range, "mi", "cot")
    | Fn_Tanh => Mml_Accum.appendOperatorOrFunction(accum, ~superscript?, ~range, "mi", "tanh")
    | Fn_Atanh => Mml_Accum.appendOperatorOrFunction(accum, ~superscript?, ~range, "mi", "arctanh")
    | Fn_Deg => Mml_Accum.appendOperatorOrFunction(accum, ~superscript?, ~range, "mi", "deg")
    | Fn_Grad => Mml_Accum.appendOperatorOrFunction(accum, ~superscript?, ~range, "mi", "grad")
    | Fn_Rad => Mml_Accum.appendOperatorOrFunction(accum, ~superscript?, ~range, "mi", "rad")
    | Fn_Log => Mml_Accum.appendOperatorOrFunction(accum, ~superscript?, ~range, "mi", "log")
    | Fn_Re => Mml_Accum.appendOperatorOrFunction(accum, ~superscript?, ~range, "mi", "re")
    | Fn_Im => Mml_Accum.appendOperatorOrFunction(accum, ~superscript?, ~range, "mi", "im")
    | Fn_Rref => Mml_Accum.appendOperatorOrFunction(accum, ~superscript?, ~range, "mi", "rref")
    | Fn_Trace => Mml_Accum.appendOperatorOrFunction(accum, ~superscript?, ~range, "mi", "tr")
    | Fn_Gamma =>
      Mml_Accum.appendOperatorOrFunction(
        accum,
        ~superscript?,
        ~range,
        ~attributes=list{(#mathvariant, "normal")},
        "mi",
        "&#x0393;",
      )
    | Fn_NLog({base}) =>
      let body = mml("mi", "log") ++ base
      Mml_Accum.appendOperatorOrFunction(accum, ~superscript?, ~range, "msub", body)
    | Fn_Sum({from, to}) | Fn_Product({from, to}) =>
      let symbol = switch x {
      | Fn_Sum(_) => "&#x2211;"
      | _ => "&#x220F;"
      }
      let body = mml("munderover", mml("mo", symbol) ++ xSetRow(from) ++ to)
      Mml_Accum.appendOperatorOrFunction(accum, ~superscript?, ~range, "mrow", body)
    }
)

%%private(
  let appendFn2 = (accum, name, a, b, superscript, range) =>
    Mml_Accum.append(
      accum,
      ~superscript?,
      ~range,
      "mrow",
      mml("mi", name) ++ mml("mo", "(") ++ a ++ mml("mo", ",") ++ b ++ mml("mo", ")"),
    )
)

%%private(
  let appendNprNcr = (accum, symbol, n, r, range) => {
    let nucleus = mml(~attributes=list{(#mathvariant, "bold")}, "mi", symbol)
    let body = mml("mmultiscripts", `${nucleus}${r}<none /><mprescripts />${n}<none />`)
    Mml_Accum.append(accum, ~range, "mrow", body)
  }
)

%%private(
  let appendDifferential = (accum, ~range, ~at, body) => {
    let pre = mml(
      "mfrac",
      mml(~attributes=list{(#mathvariant, "normal")}, "mi", "d") ++ mml("mi", "dx"),
    )
    let post = mml(~attributes=list{(#align, "left")}, "munder", mml("mo", "|") ++ xSetRow(at))
    Mml_Accum.append(accum, ~range, "mrow", pre ++ body ++ post)
  }
)

%%private(
  let appendTable = (accum, ~numRows, ~numColumns, elements, superscript, range) => {
    let inner =
      Belt.Array.makeByU(numRows, (. row) => {
        Belt.Array.makeByU(numColumns, (. column) => {
          mml("mtd", elements->Belt.Array.getUnsafe(row * numColumns + column))
        })
        ->StringUtil.join
        ->mml("mtr", _)
      })
      ->StringUtil.join
      ->mml("mtable", _)
    let body = mml("mo", "[") ++ inner ++ mml("mo", "]")
    Mml_Accum.append(accum, ~superscript?, ~range, "mrow", body)
  }
)

let reduce = (. accum, stateElement: foldState<string>, range) =>
  switch stateElement {
  | Fold_OpenBracket => Mml_Accum.appendOpenBracket(. accum, range)
  | Fold_CloseBracket(superscript) => Mml_Accum.appendCloseBracket(. accum, range, superscript)
  | Fold_Digit({nucleus, superscript}) =>
    Mml_Accum.appendDigit(accum, ~superscript?, ~range, "mn", nucleus)
  | Fold_DecimalSeparator => Mml_Accum.appendDecimalSeparator(. accum, range)
  | Fold_Base(base) => Mml_Accum.appendBasePrefix(accum, ~range, "mn", stringOfBase(base))
  | Fold_Percent => Mml_Accum.append(accum, ~avoidsSelection=true, ~range, "mo", "%")
  | Fold_Angle(Angle_Degree) => Mml_Accum.append(accum, ~range, "mo", "&#x00B0;")
  | Fold_Angle(Angle_ArcMinute) => Mml_Accum.superscriptSuffix(accum, ~range, "mo", "&#x2032;")
  | Fold_Angle(Angle_ArcSecond) => Mml_Accum.superscriptSuffix(accum, ~range, "mo", "&#x2033;")
  | Fold_Angle(Angle_Radian) =>
    let attributes = list{(#mathvariant, "normal")}
    Mml_Accum.superscriptSuffix(accum, ~attributes, ~range, "mi", "r")
  | Fold_Angle(Angle_Gradian) =>
    let attributes = list{(#mathvariant, "normal")}
    Mml_Accum.superscriptSuffix(accum, ~attributes, ~range, "mi", "g")
  | Fold_ImaginaryUnit(superscript) => Mml_Accum.append(accum, ~superscript?, ~range, "mi", "i")
  | Fold_Conj => Mml_Accum.superscriptSuffix(accum, ~range, "mo", "&#x02217;")
  | Fold_Transpose => Mml_Accum.superscriptSuffix(accum, ~range, "mi", "T")
  | Fold_Magnitude({value}) =>
    let body = mml("mrow", mml("mo", "&#x00D7;") ++ mml("mn", "10"))
    Mml_Accum.append(accum, ~range, "msup", body ++ value)
  | Fold_IterationX(superscript) => Mml_Accum.append(accum, ~superscript?, ~range, "mi", "x")
  | Fold_XUnit(superscript) => appendHat(accum, ~superscript?, ~range, "x")
  | Fold_YUnit(superscript) => appendHat(accum, ~superscript?, ~range, "y")
  | Fold_ZUnit(superscript) => appendHat(accum, ~superscript?, ~range, "z")
  | Fold_ConstPi(superscript) => Mml_Accum.append(accum, ~superscript?, ~range, "mi", "&#x03C0;")
  | Fold_ConstE(superscript) => Mml_Accum.append(accum, ~superscript?, ~range, "mi", "e")
  | Fold_Constant({symbol, superscript})
  | Fold_Variable({symbol, superscript}) =>
    Mml_Accum.append(accum, ~superscript?, ~range, "mrow", Mml_Symbol.toMml(symbol))
  | Fold_Placeholder({implicit, placeholder, superscript, captureGroupIndex}) =>
    Mml_Accum.appendPlaceholder(
      accum,
      ~range,
      ~superscript,
      ~implicit,
      ~captureGroupIndex,
      placeholder,
    )
  | Fold_Function({fn, resultSuperscript: superscript}) =>
    appendFunctionMml(accum, ~superscript?, ~range, fn)
  | Fold_Factorial => Mml_Accum.append(accum, ~avoidsSelection=true, ~range, "mo", "!")
  | Fold_Add => Mml_Accum.appendOperatorOrFunction(accum, ~avoidsSelection=true, ~range, "mo", "+")
  | Fold_Sub => Mml_Accum.appendOperatorOrFunction(accum, ~avoidsSelection=true, ~range, "mo", "-")
  | Fold_Mul =>
    Mml_Accum.appendOperatorOrFunction(accum, ~avoidsSelection=true, ~range, "mo", "&#x00D7;")
  | Fold_Div =>
    Mml_Accum.appendOperatorOrFunction(accum, ~avoidsSelection=true, ~range, "mo", "&#x00F7;")
  | Fold_Dot =>
    Mml_Accum.appendOperatorOrFunction(accum, ~avoidsSelection=true, ~range, "mo", "&#xb7;")
  | Fold_Frac({num, den, superscript}) =>
    Mml_Accum.append(accum, ~superscript?, ~range, "mfrac", num ++ den)
  | Fold_Sqrt({radicand, superscript}) =>
    Mml_Accum.append(accum, ~superscript?, ~range, "msqrt", radicand)
  | Fold_NRoot({degree, radicand, superscript}) =>
    Mml_Accum.append(accum, ~superscript?, ~range, "mroot", radicand ++ degree)
  | Fold_Abs({arg, superscript}) => appendBracketGroup(accum, "|", "|", arg, superscript, range)
  | Fold_Floor({arg, superscript}) =>
    appendBracketGroup(accum, "&#x230A;", "&#x230B;", arg, superscript, range)
  | Fold_Ceil({arg, superscript}) =>
    appendBracketGroup(accum, "&#x2308;", "&#x2309;", arg, superscript, range)
  | Fold_Round({arg, superscript}) =>
    appendBracketGroup(accum, "&#x230A;", "&#x2309;", arg, superscript, range)
  | Fold_Rand(superscript) => Mml_Accum.append(accum, ~superscript?, ~range, "mi", "rand")
  | Fold_RandInt({a, b, superscript}) =>
    let body = mml("mi", "rand#") ++ mml("mrow", a ++ mml("mo", ",") ++ b)
    let body = mml("msub", body)
    Mml_Accum.append(accum, ~superscript?, ~range, "mrow", body)
  | Fold_Min({a, b, superscript}) => appendFn2(accum, "min", a, b, superscript, range)
  | Fold_Max({a, b, superscript}) => appendFn2(accum, "max", a, b, superscript, range)
  | Fold_Gcd({a, b, superscript}) => appendFn2(accum, "gcd", a, b, superscript, range)
  | Fold_Lcm({a, b, superscript}) => appendFn2(accum, "lcm", a, b, superscript, range)
  | Fold_NPR({n, r}) => appendNprNcr(accum, "P", n, r, range)
  | Fold_NCR({n, r}) => appendNprNcr(accum, "C", n, r, range)
  | Fold_Differential({at, body}) => appendDifferential(accum, ~range, ~at, body)
  | Fold_Integral({from, to, body}) =>
    let pre = mml("msubsup", mml("mo", "&#x222B;") ++ from ++ to)
    let post = mml("mi", "dx")
    Mml_Accum.append(accum, ~range, "mrow", pre ++ body ++ post)
  | Fold_Table({elements, superscript, numRows, numColumns}) =>
    appendTable(accum, ~numRows, ~numColumns, elements, superscript, range)
  | Fold_Unit({prefix, name, superscript}) =>
    let attributes = list{(#mathvariant, "normal")}
    let body =
      TechniCalcCalculator.Formatting_Units.formatPrefix(~mode=MathML, prefix) ++
      TechniCalcCalculator.Formatting_Units.formatName(~mode=MathML, name)
    accum
    ->Mml_Accum.appendSpace(~width="0.1666em")
    // This should be mi
    // By default, any mi with a length > 1 is treated as an operator
    // MathJax has a way to disable this with data-mjx-texclass="ORD"
    // But that is broken in 3.0.1
    ->Mml_Accum.append(~attributes, ~superscript?, ~range, "mn", body)
  }

let create = (~format, ~inline=false, elements) => {
  let body = if Belt.Array.length(elements) != 0 {
    AST.reduceMapU(elements, ~reduce, ~map, ~initial=Mml_Accum.make(. format))
  } else {
    ""
  }
  mml(
    "math",
    ~attributes=list{
      (#xmlns, "http://www.w3.org/1998/Math/MathML"),
      (#display, inline ? "inline" : "block"),
    },
    body,
  )
}
