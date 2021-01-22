open AST;
open Mml_Builders;

let map = (accum, range) => Mml_Accum.toString(accum, range);

let%private bracketGroup =
            (leftBracket, rightBracket, arg, superscript, range) => {
  let body =
    createElement("mo", leftBracket)
    ++ arg
    ++ createElement("mo", rightBracket);
  elementWithRange(~superscript?, "mrow", range, body);
};

let%private sumProduct = (symbol, start, end_, range) => {
  let body =
    createElement(
      "munderover",
      createElement("mo", symbol) ++ xSetRow(start) ++ end_,
    );
  elementWithRange("mrow", range, body);
};

let%private func2 = (name, a, b, superscript, range) =>
  elementWithRange(
    ~superscript?,
    "mrow",
    range,
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
  elementWithRange("mrow", range, body);
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
  elementWithRange(~superscript?, "mrow", range, body);
};

let reduce = (accum, element: foldState(string), range) =>
  switch (element) {
  | OpenBracket => Mml_Accum.appendOpenBracket(accum, range)
  | CloseBracket(superscript) =>
    Mml_Accum.appendCloseBracket(accum, range, superscript)
  | Digit({nucleus, superscript}) =>
    elementWithRange(~superscript?, "mn", range, nucleus)
    ->Mml_Accum.appendDigit(accum, _)
  | DecimalSeparator =>
    elementWithRange("mn", range, ".")
    ->Mml_Accum.appendDecimalSeparator(accum, _)
  | Base(base) =>
    elementWithRange("mn", range, Mml_Util.stringOfBase(base))
    ->Mml_Accum.appendBasePrefix(accum, _)
  | Superscript(superscript) =>
    let placeholder =
      createElement(
        ~attributes=Placeholder.attributes,
        Placeholder.element,
        Placeholder.body,
      );
    elementWithRange("msup", range, placeholder ++ superscript)
    ->Mml_Accum.append(accum, _);
  | Percent => elementWithRange("mn", range, "%")->Mml_Accum.append(accum, _)
  | Angle(Degree) =>
    elementWithRange("mo", range, "&#x00B0;")->Mml_Accum.append(accum, _)
  | Angle(angle) =>
    let superscript =
      switch (angle) {
      | ArcMinute => createElement("mo", "&#x2032;")
      | ArcSecond => createElement("mo", "&#x2033;")
      | Gradian =>
        createElement(~attributes=[("mathvariant", "normal")], "mi", "g")
      | Degree => assert(false)
      };
    elementWithRange("msup", range, "<mo />" ++ superscript)
    ->Mml_Accum.append(accum, _);
  | ImaginaryUnit(superscript) =>
    elementWithRange(~superscript?, "mi", range, "i")
    ->Mml_Accum.append(accum, _)
  | Conj =>
    elementWithRange("mo", range, "&#x2a;")->Mml_Accum.append(accum, _)
  | Magnitude({value}) =>
    let body =
      createElement("mo", Mml_Util.stringOfOperator(Mul))
      ++ createElement("mn", "10");
    let body = createElement("mrow", body);
    elementWithRange("msup", range, body ++ value)
    ->Mml_Accum.append(accum, _);
  | Variable({nucleus, superscript}) =>
    elementWithRange(~superscript?, "mi", range, nucleus)
    ->Mml_Accum.append(accum, _)
  | ConstPi(superscript) =>
    elementWithRange(~superscript?, "mi", range, "&#x03C0;")
    ->Mml_Accum.append(accum, _)
  | ConstE(superscript) =>
    elementWithRange(~superscript?, "mi", range, "e")
    ->Mml_Accum.append(accum, _)
  | CustomAtom({mml, superscript}) =>
    elementWithRange(~superscript?, "mrow", range, mml)
    ->Mml_Accum.append(accum, _)
  | Label({mml, superscript}) =>
    elementWithRange(
      ~attributes=Placeholder.attributes,
      ~superscript?,
      "mrow",
      range,
      mml,
    )
    ->Mml_Accum.append(accum, _)
  | Function({func, resultSuperscript: superscript}) =>
    let attributes = func == AST.Gamma ? [("mathvariant", "normal")] : [];
    Mml_Util.stringOfFunction(func)
    ->elementWithRange(~superscript?, ~attributes, "mi", range, _)
    ->Mml_Accum.append(accum, _);
  | Factorial =>
    elementWithRange("mo", range, "!")->Mml_Accum.append(accum, _)
  | Operator(op) =>
    elementWithRange("mo", range, Mml_Util.stringOfOperator(op))
    ->Mml_Accum.append(accum, _)
  | Frac({num, den, superscript}) =>
    elementWithRange(~superscript?, "mfrac", range, num ++ den)
    ->Mml_Accum.append(accum, _)
  | MFrac({integer, num, den, superscript}) =>
    createElement(
      "mrow",
      integer ++ elementWithRange(~superscript?, "mfrac", range, num ++ den),
    )
    ->Mml_Accum.append(accum, _)
  | Sqrt({radicand, superscript}) =>
    elementWithRange(~superscript?, "msqrt", range, radicand)
    ->Mml_Accum.append(accum, _)
  | NRoot({degree, radicand, superscript}) =>
    elementWithRange(~superscript?, "mroot", range, radicand ++ degree)
    ->Mml_Accum.append(accum, _)
  | NLog({base}) =>
    let body = createElement("mi", "log") ++ base;
    elementWithRange("msub", range, body)->Mml_Accum.append(accum, _);
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
    elementWithRange(~superscript?, "mi", range, "rand")
    ->Mml_Accum.append(accum, _)
  | RandInt({a, b, superscript}) =>
    let body =
      createElement(
        "msub",
        createElement("mi", "rand#")
        ++ createElement("mrow", a ++ createElement("mo", ",") ++ b),
      );
    elementWithRange(~superscript?, "mrow", range, body)
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
    elementWithRange("mrow", range, pre ++ body ++ post)
    ->Mml_Accum.append(accum, _);
  | Integral({from, to_, body}) =>
    let pre =
      createElement(
        "msubsup",
        createElement("mo", "&#x222B;") ++ from ++ to_,
      );
    let post = createElement("mi", "dx");
    elementWithRange("mrow", range, pre ++ body ++ post)
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
    elementWithRange("mrow", range, body)->Mml_Accum.append(accum, _);
  };

let create = (~digitGrouping=true, ~inline=false, elements) => {
  let body =
    if (Belt.Array.length(elements) != 0) {
      AST.reduceMap(
        elements,
        ~reduce,
        ~map,
        ~initial=Mml_Accum.make(~digitGrouping),
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
