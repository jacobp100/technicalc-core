open AST
open Mml_Builders
open Mml_Util

include Mml_Types

let map = (. accum, range) => Mml_Accum.toString(accum, range)

%%private(
  let implicitMultiplication = element(
    ~attributes=Placeholder.attributes,
    "mo",
    stringOfOperator(Op_Dot),
  )
)

%%private(
  let appendElementWithImplicitMultiplication = (
    ~attributes=?,
    ~superscript=?,
    accum,
    range,
    tag,
    body,
  ) =>
    switch Mml_Accum.lastElementType(accum) {
    | Other =>
      let mml = element(~avoidsSelection=true, ~attributes?, ~superscript?, ~range, tag, body)
      Mml_Accum.append(accum, implicitMultiplication)->Mml_Accum.append(mml)
    | NoElement
    | OperatorOrFunction =>
      let mml = element(~attributes?, ~superscript?, ~range, tag, body)
      Mml_Accum.append(accum, mml)
    }
)

%%private(
  let bracketGroup = (leftBracket, rightBracket, arg, superscript, range) => {
    let body = element("mo", leftBracket) ++ arg ++ element("mo", rightBracket)
    element(~superscript?, ~range, "mrow", body)
  }
)

%%private(let xSetRow = value => element("mrow", element("mi", "x") ++ element("mo", "=") ++ value))

%%private(
  let sumProduct = (symbol, start, end_, range) => {
    let body = element("munderover", element("mo", symbol) ++ xSetRow(start) ++ end_)
    element(~range, "mrow", body)
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

let reduce = (. accum, stateElement: foldState<string>, range) =>
  switch stateElement {
  | Fold_OpenBracket => Mml_Accum.appendOpenBracket(accum, range)
  | Fold_CloseBracket(superscript) => Mml_Accum.appendCloseBracket(accum, range, superscript)
  | Fold_Digit({nucleus, superscript}) =>
    element(~superscript?, ~range, "mn", nucleus)->Mml_Accum.appendDigit(accum, _)
  | Fold_DecimalSeparator => Mml_Accum.appendDecimalSeparator(accum, range)
  | Fold_Base(base) =>
    element(~range, "mn", stringOfBase(base))->Mml_Accum.appendBasePrefix(accum, _)
  | Fold_Percent => element(~range, "mn", "%")->Mml_Accum.append(accum, _)
  | Fold_Angle(Angle_Degree) => element(~range, "mo", "&#x00B0;")->Mml_Accum.append(accum, _)
  | Fold_Angle(angle) =>
    let superscript = switch angle {
    | Angle_Radian => element(~attributes=list{(#mathvariant, "normal")}, "mi", "r")
    | Angle_ArcMinute => element("mo", "&#x2032;")
    | Angle_ArcSecond => element("mo", "&#x2033;")
    | Angle_Gradian => element(~attributes=list{(#mathvariant, "normal")}, "mi", "g")
    | Angle_Degree => assert false
    }
    element(~range, "msup", "<mo />" ++ superscript)->Mml_Accum.append(accum, _)
  | Fold_ImaginaryUnit(superscript) =>
    element(~superscript?, ~range, "mi", "i")->Mml_Accum.append(accum, _)
  | Fold_Conj => element(~range, "mo", "&#x2a;")->Mml_Accum.append(accum, _)
  | Fold_Magnitude({value}) =>
    let body = element("mo", stringOfOperator(Op_Mul)) ++ element("mn", "10")
    let body = element("mrow", body)
    element(~range, "msup", body ++ value)->Mml_Accum.append(accum, _)
  | Fold_Variable({name, superscript}) =>
    element(~superscript?, ~range, "mi", name)->Mml_Accum.append(accum, _)
  | Fold_X(superscript) => element(~superscript?, ~range, "mi", "x")->Mml_Accum.append(accum, _)
  | Fold_ConstPi(superscript) =>
    element(~superscript?, ~range, "mi", "&#x03C0;")->Mml_Accum.append(accum, _)
  | Fold_ConstE(superscript) =>
    element(~superscript?, ~range, "mi", "e")->Mml_Accum.append(accum, _)
  | Fold_CustomAtom({mml, superscript}) =>
    element(~superscript?, ~range, "mrow", mml)->Mml_Accum.append(accum, _)
  | Fold_CaptureGroupPlaceholder({placeholderMml: mml, superscript}) =>
    open Placeholder
    let phantom = element(~attributes=list{selection(~start=fst(range) + 1, ())}, "mphantom", "")
    switch mml {
    | Some(mml) =>
      let body = phantom ++ mml
      let body = element(~attributes, ~superscript?, ~range, "mrow", body)
      Mml_Accum.append(accum, body)
    | None =>
      let body = element(~attributes, ~range, tag, body)
      accum->Mml_Accum.append(phantom)->Mml_Accum.append(body)
    }
  | Fold_Placeholder(superscript) =>
    open Placeholder
    element(~attributes, ~superscript?, ~range, tag, body)->Mml_Accum.append(accum, _)
  | Fold_Function({fn, resultSuperscript: superscript}) =>
    let attributes = fn == Fn_Gamma ? list{(#mathvariant, "normal")} : list{}
    stringOfFunction(fn)
    ->element(~superscript?, ~attributes, ~range, "mi", _)
    ->Mml_Accum.appendOperatorOrFunction(accum, _)
  | Fold_Factorial => element(~range, "mo", "!")->Mml_Accum.append(accum, _)
  | Fold_Operator(op) =>
    element(~range, "mo", stringOfOperator(op))->Mml_Accum.appendOperatorOrFunction(accum, _)
  | Fold_Frac({num, den, superscript}) =>
    appendElementWithImplicitMultiplication(~superscript?, accum, range, "mfrac", num ++ den)
  | Fold_MFrac({integer, num, den, superscript}) =>
    appendElementWithImplicitMultiplication(
      ~superscript?,
      accum,
      range,
      "mrow",
      integer ++ element("mfrac", num ++ den),
    )
  | Fold_Sqrt({radicand, superscript}) =>
    element(~superscript?, ~range, "msqrt", radicand)->Mml_Accum.append(accum, _)
  | Fold_NRoot({degree, radicand, superscript}) =>
    element(~superscript?, ~range, "mroot", radicand ++ degree)->Mml_Accum.append(accum, _)

  | Fold_NLog({base}) =>
    let body = element("mi", "log") ++ base
    element(~range, "msub", body)->Mml_Accum.append(accum, _)
  | Fold_Abs({arg, superscript}) =>
    bracketGroup("|", "|", arg, superscript, range)->Mml_Accum.append(accum, _)
  | Fold_Floor({arg, superscript}) =>
    bracketGroup("&#x230A;", "&#x230B;", arg, superscript, range)->Mml_Accum.append(accum, _)
  | Fold_Ceil({arg, superscript}) =>
    bracketGroup("&#x2308;", "&#x2309;", arg, superscript, range)->Mml_Accum.append(accum, _)
  | Fold_Round({arg, superscript}) =>
    bracketGroup("&#x230A;", "&#x2309;", arg, superscript, range)->Mml_Accum.append(accum, _)
  | Fold_Rand(superscript) =>
    element(~superscript?, ~range, "mi", "rand")->Mml_Accum.append(accum, _)
  | Fold_RandInt({a, b, superscript}) =>
    let body = element("mi", "rand#") ++ element("mrow", a ++ element("mo", ",") ++ b)
    let body = element("msub", body)
    element(~superscript?, ~range, "mrow", body)->Mml_Accum.append(accum, _)
  | Fold_Min({a, b, superscript}) =>
    fn2("min", a, b, superscript, range)->Mml_Accum.append(accum, _)
  | Fold_Max({a, b, superscript}) =>
    fn2("max", a, b, superscript, range)->Mml_Accum.append(accum, _)
  | Fold_Gcd({a, b, superscript}) =>
    fn2("gcd", a, b, superscript, range)->Mml_Accum.append(accum, _)
  | Fold_Lcm({a, b, superscript}) =>
    fn2("lcm", a, b, superscript, range)->Mml_Accum.append(accum, _)
  | Fold_NPR({n, r}) => nprNcr("P", n, r, range)->Mml_Accum.append(accum, _)
  | Fold_NCR({n, r}) => nprNcr("C", n, r, range)->Mml_Accum.append(accum, _)
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
    element(~range, "mrow", pre ++ body ++ post)->Mml_Accum.append(accum, _)
  | Fold_Integral({from, to_, body}) =>
    let pre = element("msubsup", element("mo", "&#x222B;") ++ from ++ to_)
    let post = element("mi", "dx")
    element(~range, "mrow", pre ++ body ++ post)->Mml_Accum.append(accum, _)
  | Fold_Sum({from, to_}) => sumProduct("&#x2211;", from, to_, range)->Mml_Accum.append(accum, _)
  | Fold_Product({from, to_}) =>
    sumProduct("&#x220F;", from, to_, range)->Mml_Accum.append(accum, _)
  | Fold_Table({elements, superscript, numRows, numColumns}) =>
    table(~numRows, ~numColumns, elements, superscript, range)->Mml_Accum.append(accum, _)
  | Fold_UnitConversion({fromUnits, toUnits}) =>
    let body = `${Mml_Units.toMml(fromUnits)}<mo>&RightArrow;</mo>${Mml_Units.toMml(toUnits)}`
    element(~range, "mrow", body)->Mml_Accum.append(accum, _)
  }

let create = (~locale=English, ~digitGrouping=true, ~inline=false, elements) => {
  let body = if Belt.Array.length(elements) != 0 {
    AST.reduceMapU(elements, ~reduce, ~map, ~initial=Mml_Accum.make(~locale, ~digitGrouping))
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
