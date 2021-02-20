open AST;
open Mml_Builders;
open Mml_Util;

include Mml_Types;

let map = (accum, range) => Mml_Accum.toString(accum, range);

let%private implicitMultiplication =
  createElement(
    ~attributes=Placeholder.attributes,
    "mo",
    stringOfOperator(Dot),
  );

let%private appendElementWithImplicitMultiplication =
            (~attributes=[], ~superscript, accum, range, element, body) =>
  switch (Mml_Accum.lastElementType(accum)) {
  | Other =>
    let element =
      createElementWithRange(
        ~attributes=[("data-selection-before", "avoid"), ...attributes],
        ~superscript?,
        range,
        element,
        body,
      );
    Mml_Accum.append(accum, implicitMultiplication)
    ->Mml_Accum.append(element);
  | NoElement
  | OperatorOrFunction =>
    let element =
      createElementWithRange(
        ~attributes,
        ~superscript?,
        range,
        element,
        body,
      );
    Mml_Accum.append(accum, element);
  };

let%private bracketGroup =
            (leftBracket, rightBracket, arg, superscript, range) => {
  let body =
    createElement("mo", leftBracket)
    ++ arg
    ++ createElement("mo", rightBracket);
  createElementWithRange(~superscript?, range, "mrow", body);
};

let%private xSetRow = value =>
  createElement(
    "mrow",
    createElement("mi", "x") ++ createElement("mo", "=") ++ value,
  );

let%private sumProduct = (symbol, start, end_, range) => {
  let body =
    createElement(
      "munderover",
      createElement("mo", symbol) ++ xSetRow(start) ++ end_,
    );
  createElementWithRange(range, "mrow", body);
};

let%private func2 = (name, a, b, superscript, range) =>
  createElementWithRange(
    ~superscript?,
    range,
    "mrow",
    createElement("mi", name)
    ++ createElement("mo", "(")
    ++ a
    ++ createElement("mo", ",")
    ++ b
    ++ createElement("mo", ")"),
  );

let%private nprNcr = (symbol, n, r, range) => {
  let nucleus =
    createElement(~attributes=[("mathvariant", "bold")], "mi", symbol);
  let body =
    createElement(
      "mmultiscripts",
      nucleus ++ r ++ "<none /><mprescripts />" ++ n ++ "<none />",
    );
  createElementWithRange(range, "mrow", body);
};

let%private table = (~numRows, ~numColumns, elements, superscript, range) => {
  let inner =
    Belt.Array.makeBy(numRows, row => {
      Belt.Array.makeBy(numColumns, column => {
        createElement(
          "mtd",
          elements->Belt.Array.getUnsafe(row * numColumns + column),
        )
      })
      ->StringUtil.join
      ->createElement("mtr", _)
    })
    ->StringUtil.join
    ->createElement("mtable", _);
  let body = createElement("mo", "[") ++ inner ++ createElement("mo", "]");
  createElementWithRange(~superscript?, range, "mrow", body);
};

let reduce = (accum, stateElement: foldState(string), range) =>
  switch (stateElement) {
  | OpenBracket => Mml_Accum.appendOpenBracket(accum, range)
  | CloseBracket(superscript) =>
    Mml_Accum.appendCloseBracket(accum, range, superscript)
  | Digit({nucleus, superscript}) =>
    createElementWithRange(~superscript?, range, "mn", nucleus)
    ->Mml_Accum.appendDigit(accum, _)
  | DecimalSeparator =>
    Mml_Accum.appendDecimalSeparator(accum, range)
  | Base(base) =>
    createElementWithRange(range, "mn", Mml_Util.stringOfBase(base))
    ->Mml_Accum.appendBasePrefix(accum, _)
  | Percent =>
    createElementWithRange(range, "mn", "%")->Mml_Accum.append(accum, _)
  | Angle(Degree) =>
    createElementWithRange(range, "mo", "&#x00B0;")
    ->Mml_Accum.append(accum, _)
  | Angle(angle) =>
    let superscript =
      switch (angle) {
      | Radian =>
        createElement(~attributes=[("mathvariant", "normal")], "mi", "r")
      | ArcMinute => createElement("mo", "&#x2032;")
      | ArcSecond => createElement("mo", "&#x2033;")
      | Gradian =>
        createElement(~attributes=[("mathvariant", "normal")], "mi", "g")
      | Degree => assert(false)
      };
    createElementWithRange(range, "msup", "<mo />" ++ superscript)
    ->Mml_Accum.append(accum, _);
  | ImaginaryUnit(superscript) =>
    createElementWithRange(~superscript?, range, "mi", "i")
    ->Mml_Accum.append(accum, _)
  | Conj =>
    createElementWithRange(range, "mo", "&#x2a;")->Mml_Accum.append(accum, _)
  | Magnitude({value}) =>
    let body =
      createElement("mo", Mml_Util.stringOfOperator(Mul))
      ++ createElement("mn", "10");
    let body = createElement("mrow", body);
    createElementWithRange(range, "msup", body ++ value)
    ->Mml_Accum.append(accum, _);
  | Variable({name, superscript}) =>
    createElementWithRange(~superscript?, range, "mi", name)
    ->Mml_Accum.append(accum, _)
  | IteratorX(superscript) =>
    createElementWithRange(~superscript?, range, "mi", "x")
    ->Mml_Accum.append(accum, _)
  | ConstPi(superscript) =>
    createElementWithRange(~superscript?, range, "mi", "&#x03C0;")
    ->Mml_Accum.append(accum, _)
  | ConstE(superscript) =>
    createElementWithRange(~superscript?, range, "mi", "e")
    ->Mml_Accum.append(accum, _)
  | CustomAtom({mml, superscript}) =>
    createElementWithRange(~superscript?, range, "mrow", mml)
    ->Mml_Accum.append(accum, _)
  | CaptureGroupPlaceholder({placeholderMml: mml, superscript}) =>
    let attributes = Placeholder.attributes;
    let phantomId = Belt.Int.toString(fst(range) + 1) ++ ":";
    let body =
      createElement(~attributes=[("id", phantomId)], "mphantom", "") ++ mml;
    createElementWithRange(~attributes, ~superscript?, range, "mrow", body)
    ->Mml_Accum.append(accum, _);
  | Placeholder(superscript) =>
    Placeholder.(
      createElementWithRange(~attributes, ~superscript?, range, element, body)
      ->Mml_Accum.append(accum, _)
    )
  | Function({func, resultSuperscript: superscript}) =>
    let attributes = func == AST.Gamma ? [("mathvariant", "normal")] : [];
    Mml_Util.stringOfFunction(func)
    ->createElementWithRange(~superscript?, ~attributes, range, "mi", _)
    ->Mml_Accum.appendOperatorOrFunction(accum, _);
  | Factorial =>
    createElementWithRange(range, "mo", "!")->Mml_Accum.append(accum, _)
  | Operator(op) =>
    createElementWithRange(range, "mo", Mml_Util.stringOfOperator(op))
    ->Mml_Accum.appendOperatorOrFunction(accum, _)
  | Frac({num, den, superscript}) =>
    appendElementWithImplicitMultiplication(
      ~superscript,
      accum,
      range,
      "mfrac",
      num ++ den,
    )
  | MFrac({integer, num, den, superscript}) =>
    appendElementWithImplicitMultiplication(
      ~superscript,
      accum,
      range,
      "mrow",
      integer ++ createElement("mfrac", num ++ den),
    )
  | Sqrt({radicand, superscript}) =>
    createElementWithRange(~superscript?, range, "msqrt", radicand)
    ->Mml_Accum.append(accum, _)
  | NRoot({degree, radicand, superscript}) =>
    createElementWithRange(~superscript?, range, "mroot", radicand ++ degree)
    ->Mml_Accum.append(accum, _)
  | NLog({base}) =>
    let body = createElement("mi", "log") ++ base;
    createElementWithRange(range, "msub", body)->Mml_Accum.append(accum, _);
  | Abs({arg, superscript}) =>
    bracketGroup("|", "|", arg, superscript, range)
    ->Mml_Accum.append(accum, _)
  | Floor({arg, superscript}) =>
    bracketGroup("&#x230A;", "&#x230B;", arg, superscript, range)
    ->Mml_Accum.append(accum, _)
  | Ceil({arg, superscript}) =>
    bracketGroup("&#x2308;", "&#x2309;", arg, superscript, range)
    ->Mml_Accum.append(accum, _)
  | Round({arg, superscript}) =>
    bracketGroup("&#x230A;", "&#x2309;", arg, superscript, range)
    ->Mml_Accum.append(accum, _)
  | Rand(superscript) =>
    createElementWithRange(~superscript?, range, "mi", "rand")
    ->Mml_Accum.append(accum, _)
  | RandInt({a, b, superscript}) =>
    let body =
      createElement("mi", "rand#")
      ++ createElement("mrow", a ++ createElement("mo", ",") ++ b);
    let body = createElement("msub", body);
    createElementWithRange(~superscript?, range, "mrow", body)
    ->Mml_Accum.append(accum, _);
  | Min({a, b, superscript}) =>
    func2("min", a, b, superscript, range)->Mml_Accum.append(accum, _)
  | Max({a, b, superscript}) =>
    func2("max", a, b, superscript, range)->Mml_Accum.append(accum, _)
  | Gcd({a, b, superscript}) =>
    func2("gcd", a, b, superscript, range)->Mml_Accum.append(accum, _)
  | Lcm({a, b, superscript}) =>
    func2("lcm", a, b, superscript, range)->Mml_Accum.append(accum, _)
  | NPR({n, r}) => nprNcr("P", n, r, range)->Mml_Accum.append(accum, _)
  | NCR({n, r}) => nprNcr("C", n, r, range)->Mml_Accum.append(accum, _)
  | Differential({at, body}) =>
    let pre =
      createElement(
        "mfrac",
        createElement(~attributes=[("mathvariant", "normal")], "mi", "d")
        ++ createElement("mi", "dx"),
      );
    let post =
      createElement(
        ~attributes=[("align", "left")],
        "munder",
        createElement("mo", "|") ++ xSetRow(at),
      );
    createElementWithRange(range, "mrow", pre ++ body ++ post)
    ->Mml_Accum.append(accum, _);
  | Integral({from, to_, body}) =>
    let pre =
      createElement(
        "msubsup",
        createElement("mo", "&#x222B;") ++ from ++ to_,
      );
    let post = createElement("mi", "dx");
    createElementWithRange(range, "mrow", pre ++ body ++ post)
    ->Mml_Accum.append(accum, _);
  | Sum({from, to_}) =>
    sumProduct("&#x2211;", from, to_, range)->Mml_Accum.append(accum, _)
  | Product({from, to_}) =>
    sumProduct("&#x220F;", from, to_, range)->Mml_Accum.append(accum, _)
  | Vector({elements, superscript}) =>
    let numRows = Belt.Array.length(elements);
    table(~numRows, ~numColumns=1, elements, superscript, range)
    ->Mml_Accum.append(accum, _);
  | Table({elements, superscript, numRows, numColumns}) =>
    table(~numRows, ~numColumns, elements, superscript, range)
    ->Mml_Accum.append(accum, _)
  | UnitConversion({fromUnits, toUnits}) =>
    let body =
      Mml_Units.toMml(fromUnits)
      ++ "<mo>&RightArrow;</mo>"
      ++ Mml_Units.toMml(toUnits);
    createElementWithRange(range, "mrow", body)->Mml_Accum.append(accum, _);
  };

let create = (~locale=English, ~digitGrouping=true, ~inline=false, elements) => {
  let body =
    if (Belt.Array.length(elements) != 0) {
      AST.reduceMap(
        elements,
        ~reduce,
        ~map,
        ~initial=Mml_Accum.make(~locale, ~digitGrouping),
      );
    } else {
      "";
    };
  createElement(
    "math",
    ~attributes=[
      ("xmlns", "http://www.w3.org/1998/Math/MathML"),
      ("display", inline ? "inline" : "block"),
    ],
    body,
  );
};
