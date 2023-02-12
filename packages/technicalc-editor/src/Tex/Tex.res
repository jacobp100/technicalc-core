open AST

%%private(
  let stringOfBase = base =>
    switch base {
    | Base_Bin => "\\rm{0b}"
    | Base_Oct => "\\rm{0o}"
    | Base_Hex => "\\rm{0x}"
    }
)

%%private(let withSpaces = body => ` ${body} `)

%%private(
  let withSuperscript = (body, superscript) =>
    switch superscript {
    | Some({AST.superscriptBody: superscriptBody}) => `${body}^${superscriptBody}`
    | None => body
    }
)

%%private(let removeBraces = body => StringUtil.slice(body, 1, -1)->StringUtil.trim)

%%private(
  let functionTex = x =>
    switch x {
    | Fn_Sin => "\\sin"
    | Fn_Asin => "\\arcsin"
    | Fn_Cosec => "\\cosec"
    | Fn_Sinh => "\\sinh"
    | Fn_Asinh => "\\arcsinh"
    | Fn_Cos => "\\cos"
    | Fn_Acos => "\\arccos"
    | Fn_Sec => "\\sec"
    | Fn_Cosh => "\\cosh"
    | Fn_Acosh => "\\arccosh"
    | Fn_Tan => "\\tan"
    | Fn_Atan => "\\arctan"
    | Fn_Cot => "\\cot"
    | Fn_Tanh => "\\tanh"
    | Fn_Atanh => "\\arctanh"
    | Fn_Deg => "\\deg"
    | Fn_Grad => "\\grad"
    | Fn_Rad => "\\rm{rad}"
    | Fn_Log => "\\log"
    | Fn_Re => "\\rm{re}"
    | Fn_Im => "\\rm{im}"
    | Fn_Rref => "\\rm{rref}"
    | Fn_Trace => "\\rm{trace}"
    | Fn_Gamma => "\\Gamma"
    | Fn_NLog({base}) => `\\log_${base}`
    | Fn_Sum({from, to})
    | Fn_Product({from, to}) =>
      let symbol = switch x {
      | Fn_Sum(_) => "\\sum"
      | _ => "\\prod"
      }
      `${symbol}_{x=${removeBraces(from)}}^${to}`
    }
)

%%private(
  let fn2 = (name, a, b, superscript) =>
    withSuperscript(`${name} \\left(${removeBraces(a)}, ${removeBraces(b)}\\right)`, superscript)
)

%%private(let nprNcr = (symbol, n, r) => `{}_${n}${symbol}{}_${r}`)

%%private(
  let table = (~numRows, ~numColumns, elements, superscript) => {
    let body = Belt.Array.makeByU(numRows, (. row) => {
      Belt.Array.makeByU(numColumns, (. column) => {
        Belt.Array.getUnsafe(elements, row * numColumns + column)->removeBraces
      })->StringUtil.joinWith(" & ")
    })->StringUtil.joinWith(" \\\\ ")

    `\\left[ \\begin{matrix} ${body} \\end{matrix} ${withSuperscript(`\\right]`, superscript)}`
  }
)

module Tex_Accum = Stringifier.Make({
  let groupingSeparatorU = (. groupingSeparator) =>
    groupingSeparator == " " ? `\\,` : groupingSeparator
  let decimalSeparatorU = (. decimalSeparator, _, _) => decimalSeparator

  let unpairedOpenBracketU = (. _, _) => " ( "
  let unpairedCloseBracketU = (. superscript, _, _) => withSuperscript(" ) ", superscript)

  let bracketRangeU = (. superscript, body, _, _, _) =>
    withSpaces(`\\left( ${body} \\right)`)->withSuperscript(superscript)
})

%%private(
  let supsrscriptSuffix = (accum, body) =>
    Tex_Accum.modifyLastU(.accum, (. last) => {
      let prev = Belt.Option.getWithDefault(last, "{}")->StringUtil.trim
      `${prev}^{${body}}`
    })
)

let map = (. accum, _) => `{${Tex_Accum.toString(. accum)->StringUtil.trim}}`

let reduce = (. accum, stateElement: foldState<string>, range) =>
  switch stateElement {
  | Fold_OpenBracket => Tex_Accum.appendOpenBracket(. accum, range)
  | Fold_CloseBracket(superscript) => Tex_Accum.appendCloseBracket(. accum, range, superscript)
  | Fold_Digit({nucleus, superscript}) =>
    withSuperscript(nucleus, superscript)->Tex_Accum.appendDigit(. accum, _)
  | Fold_DecimalSeparator => Tex_Accum.appendDecimalSeparator(. accum, range)
  | Fold_Base(base) => stringOfBase(base)->Tex_Accum.appendBasePrefix(. accum, _)
  | Fold_Percent => "\\%"->Tex_Accum.append(. accum, _)
  | Fold_Angle(Angle_Degree) => supsrscriptSuffix(accum, "\\circ")
  | Fold_Angle(Angle_Radian) => supsrscriptSuffix(accum, "r")
  | Fold_Angle(Angle_Gradian) => supsrscriptSuffix(accum, "g")
  | Fold_Angle(Angle_ArcMinute) => "'"->Tex_Accum.append(. accum, _)
  | Fold_Angle(Angle_ArcSecond) => "''"->Tex_Accum.append(. accum, _)
  | Fold_ImaginaryUnit(superscript) =>
    withSuperscript("i", superscript)->Tex_Accum.append(. accum, _)
  | Fold_Conj => supsrscriptSuffix(accum, "*")
  | Fold_Transpose => supsrscriptSuffix(accum, "T")
  | Fold_Magnitude({value}) => `\\times 10^${value}`->Tex_Accum.append(. accum, _)
  | Fold_IterationX(superscript) => withSuperscript("x", superscript)->Tex_Accum.append(. accum, _)
  | Fold_XUnit(superscript) =>
    withSuperscript("\\hat{x}", superscript)->Tex_Accum.append(. accum, _)
  | Fold_YUnit(superscript) =>
    withSuperscript("\\hat{y}", superscript)->Tex_Accum.append(. accum, _)
  | Fold_ZUnit(superscript) =>
    withSuperscript("\\hat{z}", superscript)->Tex_Accum.append(. accum, _)
  | Fold_ConstPi(superscript) => withSuperscript("\\pi", superscript)->Tex_Accum.append(. accum, _)
  | Fold_ConstE(superscript) => withSuperscript("e", superscript)->Tex_Accum.append(. accum, _)
  | Fold_Variable({symbol, superscript})
  | Fold_Constant({symbol, superscript}) =>
    Tex_Symbol.toTex(symbol)->withSuperscript(superscript)->Tex_Accum.append(. accum, _)
  | Fold_Placeholder({placeholder, superscript}) =>
    let placeholder = switch placeholder {
    | Some(placeholder) => Tex_Symbol.toTex(placeholder)
    | None => "{}"
    }
    withSuperscript(placeholder, superscript)->Tex_Accum.append(. accum, _)
  | Fold_Function({fn, resultSuperscript: superscript}) =>
    functionTex(fn)
    ->withSuperscript(superscript)
    ->withSpaces
    ->Tex_Accum.appendOperatorOrFunction(. accum, _)
  | Fold_Factorial => "!"->Tex_Accum.append(. accum, _)
  | Fold_Add => withSpaces("+")->Tex_Accum.appendOperatorOrFunction(. accum, _)
  | Fold_Sub => withSpaces("-")->Tex_Accum.appendOperatorOrFunction(. accum, _)
  | Fold_Mul => withSpaces("\\times")->Tex_Accum.appendOperatorOrFunction(. accum, _)
  | Fold_Div => withSpaces("\\div")->Tex_Accum.appendOperatorOrFunction(. accum, _)
  | Fold_Dot => withSpaces("\\cdot")->Tex_Accum.appendOperatorOrFunction(. accum, _)
  | Fold_Frac({num, den, superscript}) =>
    withSuperscript(`\\frac${num}${den}`, superscript)->withSpaces->Tex_Accum.append(. accum, _)
  | Fold_Sqrt({radicand, superscript}) =>
    withSuperscript(`\\sqrt${radicand}`, superscript)->withSpaces->Tex_Accum.append(. accum, _)
  | Fold_NRoot({degree, radicand, superscript}) =>
    withSuperscript(`\\sqrt[${degree}]${radicand}`, superscript)
    ->withSpaces
    ->Tex_Accum.append(. accum, _)
  | Fold_Abs({arg, superscript}) =>
    withSuperscript(`\\left|${arg}\\right|`, superscript)->Tex_Accum.append(. accum, _)
  | Fold_Floor({arg, superscript}) =>
    withSuperscript(`\\lfloor|${arg}\\rfloor|`, superscript)->Tex_Accum.append(. accum, _)
  | Fold_Ceil({arg, superscript}) =>
    withSuperscript(`\\lceil|${arg}\\rceil|`, superscript)->Tex_Accum.append(. accum, _)
  | Fold_Round({arg, superscript}) =>
    withSuperscript(`\\lfloor|${arg}\\rceil|`, superscript)->Tex_Accum.append(. accum, _)
  | Fold_Rand(superscript) =>
    withSuperscript(`\\rm{rand}`, superscript)->withSpaces->Tex_Accum.append(. accum, _)
  | Fold_RandInt({a, b, superscript}) =>
    withSuperscript(`\\rm{rand}_{\#${a},${b}}`, superscript)
    ->withSpaces
    ->Tex_Accum.append(. accum, _)
  | Fold_Min({a, b, superscript}) => fn2("\\min", a, b, superscript)->Tex_Accum.append(. accum, _)
  | Fold_Max({a, b, superscript}) => fn2("\\max", a, b, superscript)->Tex_Accum.append(. accum, _)
  | Fold_Gcd({a, b, superscript}) => fn2("\\gcd", a, b, superscript)->Tex_Accum.append(. accum, _)
  | Fold_Lcm({a, b, superscript}) =>
    fn2("\\rm{lcm}", a, b, superscript)->Tex_Accum.append(. accum, _)
  | Fold_NPR({n, r}) => nprNcr("P", n, r)->withSpaces->Tex_Accum.append(. accum, _)
  | Fold_NCR({n, r}) => nprNcr("C", n, r)->withSpaces->Tex_Accum.append(. accum, _)
  | Fold_Differential({at, body}) =>
    `\\left.\\frac{d}{dx}${body}\\right|_{x=${removeBraces(at)}}`
    ->withSpaces
    ->Tex_Accum.append(. accum, _)
  | Fold_Integral({from, to, body}) =>
    `\\int_${from}^${to} ${body} dx`->withSpaces->Tex_Accum.append(. accum, _)
  | Fold_Table({elements, superscript, numRows, numColumns}) =>
    table(~numRows, ~numColumns, elements, superscript)->withSpaces->Tex_Accum.append(. accum, _)
  }

let create = (~format, elements) =>
  if Belt.Array.length(elements) != 0 {
    AST.reduceMapU(elements, ~reduce, ~map, ~initial=Tex_Accum.make(. format))->removeBraces
  } else {
    ""
  }
