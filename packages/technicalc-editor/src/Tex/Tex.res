open AST
open Tex_Util

let withSuperscript = (body, superscript) =>
  switch superscript {
  | Some({AST.superscriptBody: superscriptBody}) => `${body}^${superscriptBody}`
  | None => body
  }

module Tex_Accum = Stringifier.Make({
  let groupingSeparatorU = (. locale) => Stringifier.groupingSeparator(locale)
  let decimalSeparatorU = (. locale, _) => Stringifier.decimalSeparator(locale)

  let unpairedOpenBracketU = (. _) => "("
  let unpairedCloseBracketU = (. superscript, _) => withSuperscript(")", superscript)

  let bracketRangeU = (. superscript, body, _, _) =>
    withSuperscript(`\\left(${body}\\right)`, superscript)
})

let map = (. accum, _) => `{${Tex_Accum.toString(. accum)}}`

%%private(let sumProduct = (symbol, start, end) => `${symbol}_{x=${start}}^${end}`)

%%private(
  let fn2 = (name, a, b, superscript) =>
    withSuperscript(`${name}\\left(${a}, ${b}\\right)`, superscript)
)

%%private(let nprNcr = (symbol, n, r) => `{}_${n}${symbol}{}_${r}`)

%%private(
  let table = (~numRows, ~numColumns, elements, superscript) => {
    let body = Belt.Array.makeByU(numRows, (. row) => {
      Belt.Array.makeByU(numColumns, (. column) => {
        Belt.Array.getUnsafe(elements, row * numColumns + column)
      })->StringUtil.joinWith(" & ")
    })->StringUtil.joinWith(" \\\\ ")

    `\\left[\\begin{matrix}${body}\\end{matrix}${withSuperscript(`\\right]`, superscript)}`
  }
)

let reduce = (. accum, stateElement: foldState<string>, range) =>
  switch stateElement {
  | Fold_OpenBracket => Tex_Accum.appendOpenBracket(. accum, range)
  | Fold_CloseBracket(superscript) => Tex_Accum.appendCloseBracket(. accum, range, superscript)
  | Fold_Digit({nucleus, superscript}) =>
    withSuperscript(nucleus, superscript)->Tex_Accum.appendDigit(. accum, _)
  | Fold_DecimalSeparator => Tex_Accum.appendDecimalSeparator(. accum, range)
  | Fold_Base(base) => stringOfBase(base)->Tex_Accum.appendBasePrefix(. accum, _)
  | Fold_Percent => "\\%"->Tex_Accum.append(. accum, _)
  | Fold_Angle(Angle_Degree) => "{}^{\\circ}"->Tex_Accum.append(. accum, _)
  | Fold_Angle(Angle_Radian) => "{}^{r}"->Tex_Accum.append(. accum, _)
  | Fold_Angle(Angle_Gradian) => "{}^{g}"->Tex_Accum.append(. accum, _)
  | Fold_Angle(Angle_ArcMinute) => "'"->Tex_Accum.append(. accum, _)
  | Fold_Angle(Angle_ArcSecond) => "''"->Tex_Accum.append(. accum, _)
  | Fold_ImaginaryUnit(superscript) =>
    withSuperscript("i", superscript)->Tex_Accum.append(. accum, _)
  | Fold_Conj => "{}^{*}"->Tex_Accum.append(. accum, _)
  | Fold_Magnitude({value}) => `\\times 10^${value}`->Tex_Accum.append(. accum, _)
  | Fold_Variable({name, superscript}) =>
    withSuperscript(name, superscript)->Tex_Accum.append(. accum, _)
  | Fold_X(superscript) => withSuperscript("x", superscript)->Tex_Accum.append(. accum, _)
  | Fold_ConstPi(superscript) => withSuperscript("\\pi", superscript)->Tex_Accum.append(. accum, _)
  | Fold_ConstE(superscript) => withSuperscript("e", superscript)->Tex_Accum.append(. accum, _)
  | Fold_CustomAtom({mml: _mml, superscript}) =>
    withSuperscript("???", superscript)->Tex_Accum.append(. accum, _)
  | Fold_CaptureGroupPlaceholder({placeholderMml: _mml, superscript}) =>
    withSuperscript("???", superscript)->Tex_Accum.append(. accum, _)
  | Fold_Placeholder(_superscript) => "{}"->Tex_Accum.append(. accum, _)
  | Fold_Function({fn, resultSuperscript: superscript}) =>
    stringOfFunction(fn)
    ->withSuperscript(superscript)
    ->Tex_Accum.appendOperatorOrFunction(. accum, _)
  | Fold_Factorial => "!"->Tex_Accum.append(. accum, _)
  | Fold_Operator(op) => stringOfOperator(op)->Tex_Accum.appendOperatorOrFunction(. accum, _)
  | Fold_Frac({num, den, superscript}) =>
    withSuperscript(`\\frac${num}${den}`, superscript)->Tex_Accum.append(. accum, _)
  | Fold_MFrac({integer, num, den, superscript}) =>
    withSuperscript(`${integer}\\frac${num}${den}`, superscript)->Tex_Accum.append(. accum, _)
  | Fold_Sqrt({radicand, superscript}) =>
    withSuperscript(`\\sqrt${radicand}`, superscript)->Tex_Accum.append(. accum, _)
  | Fold_NRoot({degree, radicand, superscript}) =>
    withSuperscript(`\\sqrt[${degree}]${radicand}`, superscript)->Tex_Accum.append(. accum, _)
  | Fold_NLog({base}) => `\\log_${base}`->Tex_Accum.append(. accum, _)
  | Fold_Abs({arg, superscript}) =>
    withSuperscript(`\\left|${arg}\\right|`, superscript)->Tex_Accum.append(. accum, _)
  | Fold_Floor({arg, superscript}) =>
    withSuperscript(`\\lfloor|${arg}\\rfloor|`, superscript)->Tex_Accum.append(. accum, _)
  | Fold_Ceil({arg, superscript}) =>
    withSuperscript(`\\lceil|${arg}\\rceil|`, superscript)->Tex_Accum.append(. accum, _)
  | Fold_Round({arg, superscript}) =>
    withSuperscript(`\\lfloor|${arg}\\rceil|`, superscript)->Tex_Accum.append(. accum, _)
  | Fold_Rand(superscript) =>
    withSuperscript(`\\rm{rand}`, superscript)->Tex_Accum.append(. accum, _)
  | Fold_RandInt({a, b, superscript}) =>
    withSuperscript(`\\rm{rand}_{\#${a},${b}}`, superscript)->Tex_Accum.append(. accum, _)
  | Fold_Min({a, b, superscript}) => fn2("\\min", a, b, superscript)->Tex_Accum.append(. accum, _)
  | Fold_Max({a, b, superscript}) => fn2("\\max", a, b, superscript)->Tex_Accum.append(. accum, _)
  | Fold_Gcd({a, b, superscript}) => fn2("\\gcd", a, b, superscript)->Tex_Accum.append(. accum, _)
  | Fold_Lcm({a, b, superscript}) =>
    fn2("\\rm{lcm}", a, b, superscript)->Tex_Accum.append(. accum, _)
  | Fold_NPR({n, r}) => nprNcr("P", n, r)->Tex_Accum.append(. accum, _)
  | Fold_NCR({n, r}) => nprNcr("C", n, r)->Tex_Accum.append(. accum, _)
  | Fold_Differential({at, body}) =>
    `\\left.\\frac{d}{dx}${body}\\right|_{x=${at}}`->Tex_Accum.append(. accum, _)
  | Fold_Integral({from, to, body}) =>
    `\\int_${from}^${to} ${body} dx`->Tex_Accum.append(. accum, _)
  | Fold_Sum({from, to}) => sumProduct("\\sum", from, to)->Tex_Accum.append(. accum, _)
  | Fold_Product({from, to}) => sumProduct("\\prod", from, to)->Tex_Accum.append(. accum, _)
  | Fold_Table({elements, superscript, numRows, numColumns}) =>
    table(~numRows, ~numColumns, elements, superscript)->Tex_Accum.append(. accum, _)
  | Fold_UnitConversion(_) => "???"->Tex_Accum.append(. accum, _)
  }

let create = (~locale=Stringifier.English, ~digitGrouping=true, elements) =>
  if Belt.Array.length(elements) != 0 {
    AST.reduceMapU(
      elements,
      ~reduce,
      ~map,
      ~initial=Tex_Accum.make(. locale, digitGrouping),
    )->StringUtil.slice(1, -1)
  } else {
    ""
  }
