open AST
open Mml_Builders
open Mml_Util

include Mml_Types

let map = (. accum, range) => Mml_Accum.toString(accum, range)

%%private(
  let implicitMultiplication = createElement(
    ~attributes=Placeholder.attributes,
    "mo",
    stringOfOperator(Op_Dot),
  )
)

%%private(
  let appendElementWithImplicitMultiplication = (
    ~attributes=list{},
    ~superscript,
    accum,
    range,
    element,
    body,
  ) =>
    switch Mml_Accum.lastElementType(accum) {
    | Other =>
      let element = createElementWithRange(
        ~attributes=list{("data-selection-before", "avoid"), ...attributes},
        ~superscript?,
        range,
        element,
        body,
      )
      Mml_Accum.append(accum, implicitMultiplication)->Mml_Accum.append(element)
    | NoElement
    | OperatorOrFunction =>
      let element = createElementWithRange(~attributes, ~superscript?, range, element, body)
      Mml_Accum.append(accum, element)
    }
)

%%private(
  let bracketGroup = (leftBracket, rightBracket, arg, superscript, range) => {
    let body = createElement("mo", leftBracket) ++ arg ++ createElement("mo", rightBracket)
    createElementWithRange(~superscript?, range, "mrow", body)
  }
)

%%private(
  let xSetRow = value =>
    createElement("mrow", createElement("mi", "x") ++ createElement("mo", "=") ++ value)
)

%%private(
  let sumProduct = (symbol, start, end_, range) => {
    let body = createElement("munderover", createElement("mo", symbol) ++ xSetRow(start) ++ end_)
    createElementWithRange(range, "mrow", body)
  }
)

%%private(
  let fn2 = (name, a, b, superscript, range) =>
    createElementWithRange(
      ~superscript?,
      range,
      "mrow",
      createElement("mi", name) ++
      createElement("mo", "(") ++
      a ++
      createElement("mo", ",") ++
      b ++
      createElement("mo", ")"),
    )
)

%%private(
  let nprNcr = (symbol, n, r, range) => {
    let nucleus = createElement(~attributes=list{("mathvariant", "bold")}, "mi", symbol)
    let body = createElement("mmultiscripts", `${nucleus}${r}<none /><mprescripts />${n}<none />`)
    createElementWithRange(range, "mrow", body)
  }
)

%%private(
  let table = (~numRows, ~numColumns, elements, superscript, range) => {
    let inner =
      Belt.Array.makeByU(numRows, (. row) => {
        Belt.Array.makeByU(numColumns, (. column) => {
          createElement("mtd", elements->Belt.Array.getUnsafe(row * numColumns + column))
        })
        ->StringUtil.join
        ->createElement("mtr", _)
      })
      ->StringUtil.join
      ->createElement("mtable", _)
    let body = createElement("mo", "[") ++ inner ++ createElement("mo", "]")
    createElementWithRange(~superscript?, range, "mrow", body)
  }
)

let reduce = (. accum, stateElement: foldState<string>, range) =>
  switch stateElement {
  | Fold_OpenBracket => Mml_Accum.appendOpenBracket(accum, range)
  | Fold_CloseBracket(superscript) => Mml_Accum.appendCloseBracket(accum, range, superscript)
  | Fold_Digit({nucleus, superscript}) =>
    createElementWithRange(~superscript?, range, "mn", nucleus)->Mml_Accum.appendDigit(accum, _)
  | Fold_DecimalSeparator => Mml_Accum.appendDecimalSeparator(accum, range)
  | Fold_Base(base) =>
    createElementWithRange(range, "mn", Mml_Util.stringOfBase(base))->Mml_Accum.appendBasePrefix(
      accum,
      _,
    )

  | Fold_Percent => createElementWithRange(range, "mn", "%")->Mml_Accum.append(accum, _)
  | Fold_Angle(Angle_Degree) =>
    createElementWithRange(range, "mo", "&#x00B0;")->Mml_Accum.append(accum, _)
  | Fold_Angle(angle) =>
    let superscript = switch angle {
    | Angle_Radian => createElement(~attributes=list{("mathvariant", "normal")}, "mi", "r")
    | Angle_ArcMinute => createElement("mo", "&#x2032;")
    | Angle_ArcSecond => createElement("mo", "&#x2033;")
    | Angle_Gradian => createElement(~attributes=list{("mathvariant", "normal")}, "mi", "g")
    | Angle_Degree => assert false
    }
    createElementWithRange(range, "msup", "<mo />" ++ superscript)->Mml_Accum.append(accum, _)
  | Fold_ImaginaryUnit(superscript) =>
    createElementWithRange(~superscript?, range, "mi", "i")->Mml_Accum.append(accum, _)
  | Fold_Conj => createElementWithRange(range, "mo", "&#x2a;")->Mml_Accum.append(accum, _)
  | Fold_Magnitude({value}) =>
    let body = createElement("mo", Mml_Util.stringOfOperator(Op_Mul)) ++ createElement("mn", "10")
    let body = createElement("mrow", body)
    createElementWithRange(range, "msup", body ++ value)->Mml_Accum.append(accum, _)
  | Fold_Variable({name, superscript}) =>
    createElementWithRange(~superscript?, range, "mi", name)->Mml_Accum.append(accum, _)
  | Fold_X(superscript) =>
    createElementWithRange(~superscript?, range, "mi", "x")->Mml_Accum.append(accum, _)
  | Fold_ConstPi(superscript) =>
    createElementWithRange(~superscript?, range, "mi", "&#x03C0;")->Mml_Accum.append(accum, _)
  | Fold_ConstE(superscript) =>
    createElementWithRange(~superscript?, range, "mi", "e")->Mml_Accum.append(accum, _)
  | Fold_CustomAtom({mml, superscript}) =>
    createElementWithRange(~superscript?, range, "mrow", mml)->Mml_Accum.append(accum, _)
  | Fold_CaptureGroupPlaceholder({placeholderMml: mml, superscript}) =>
    let attributes = Placeholder.attributes
    let phantomId = Belt.Int.toString(fst(range) + 1) ++ ":"
    let body = createElement(~attributes=list{("id", phantomId)}, "mphantom", "") ++ mml
    createElementWithRange(~attributes, ~superscript?, range, "mrow", body)->Mml_Accum.append(
      accum,
      _,
    )
  | Fold_Placeholder(superscript) =>
    open Placeholder
    createElementWithRange(~attributes, ~superscript?, range, element, body)->Mml_Accum.append(
      accum,
      _,
    )
  | Fold_Function({fn, resultSuperscript: superscript}) =>
    let attributes = fn == Fn_Gamma ? list{("mathvariant", "normal")} : list{}
    Mml_Util.stringOfFunction(fn)
    ->createElementWithRange(~superscript?, ~attributes, range, "mi", _)
    ->Mml_Accum.appendOperatorOrFunction(accum, _)
  | Fold_Factorial => createElementWithRange(range, "mo", "!")->Mml_Accum.append(accum, _)
  | Fold_Operator(op) =>
    createElementWithRange(
      range,
      "mo",
      Mml_Util.stringOfOperator(op),
    )->Mml_Accum.appendOperatorOrFunction(accum, _)
  | Fold_Frac({num, den, superscript}) =>
    appendElementWithImplicitMultiplication(~superscript, accum, range, "mfrac", num ++ den)
  | Fold_MFrac({integer, num, den, superscript}) =>
    appendElementWithImplicitMultiplication(
      ~superscript,
      accum,
      range,
      "mrow",
      integer ++ createElement("mfrac", num ++ den),
    )
  | Fold_Sqrt({radicand, superscript}) =>
    createElementWithRange(~superscript?, range, "msqrt", radicand)->Mml_Accum.append(accum, _)
  | Fold_NRoot({degree, radicand, superscript}) =>
    createElementWithRange(~superscript?, range, "mroot", radicand ++ degree)->Mml_Accum.append(
      accum,
      _,
    )

  | Fold_NLog({base}) =>
    let body = createElement("mi", "log") ++ base
    createElementWithRange(range, "msub", body)->Mml_Accum.append(accum, _)
  | Fold_Abs({arg, superscript}) =>
    bracketGroup("|", "|", arg, superscript, range)->Mml_Accum.append(accum, _)
  | Fold_Floor({arg, superscript}) =>
    bracketGroup("&#x230A;", "&#x230B;", arg, superscript, range)->Mml_Accum.append(accum, _)
  | Fold_Ceil({arg, superscript}) =>
    bracketGroup("&#x2308;", "&#x2309;", arg, superscript, range)->Mml_Accum.append(accum, _)
  | Fold_Round({arg, superscript}) =>
    bracketGroup("&#x230A;", "&#x2309;", arg, superscript, range)->Mml_Accum.append(accum, _)
  | Fold_Rand(superscript) =>
    createElementWithRange(~superscript?, range, "mi", "rand")->Mml_Accum.append(accum, _)
  | Fold_RandInt({a, b, superscript}) =>
    let body =
      createElement("mi", "rand#") ++ createElement("mrow", a ++ createElement("mo", ",") ++ b)
    let body = createElement("msub", body)
    createElementWithRange(~superscript?, range, "mrow", body)->Mml_Accum.append(accum, _)
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
    let pre = createElement(
      "mfrac",
      createElement(~attributes=list{("mathvariant", "normal")}, "mi", "d") ++
      createElement("mi", "dx"),
    )
    let post = createElement(
      ~attributes=list{("align", "left")},
      "munder",
      createElement("mo", "|") ++ xSetRow(at),
    )
    createElementWithRange(range, "mrow", pre ++ body ++ post)->Mml_Accum.append(accum, _)
  | Fold_Integral({from, to_, body}) =>
    let pre = createElement("msubsup", createElement("mo", "&#x222B;") ++ from ++ to_)
    let post = createElement("mi", "dx")
    createElementWithRange(range, "mrow", pre ++ body ++ post)->Mml_Accum.append(accum, _)
  | Fold_Sum({from, to_}) => sumProduct("&#x2211;", from, to_, range)->Mml_Accum.append(accum, _)
  | Fold_Product({from, to_}) =>
    sumProduct("&#x220F;", from, to_, range)->Mml_Accum.append(accum, _)
  | Fold_Vector({elements, superscript}) =>
    let numRows = Belt.Array.length(elements)
    table(~numRows, ~numColumns=1, elements, superscript, range)->Mml_Accum.append(accum, _)
  | Fold_Table({elements, superscript, numRows, numColumns}) =>
    table(~numRows, ~numColumns, elements, superscript, range)->Mml_Accum.append(accum, _)
  | Fold_UnitConversion({fromUnits, toUnits}) =>
    let body = `${Mml_Units.toMml(fromUnits)}<mo>&RightArrow;</mo>${Mml_Units.toMml(toUnits)}`
    createElementWithRange(range, "mrow", body)->Mml_Accum.append(accum, _)
  }

let create = (~locale=English, ~digitGrouping=true, ~inline=false, elements) => {
  let body = if Belt.Array.length(elements) != 0 {
    AST.reduceMapU(elements, ~reduce, ~map, ~initial=Mml_Accum.make(~locale, ~digitGrouping))
  } else {
    ""
  }
  createElement(
    "math",
    ~attributes=list{
      ("xmlns", "http://www.w3.org/1998/Math/MathML"),
      ("display", inline ? "inline" : "block"),
    },
    body,
  )
}
