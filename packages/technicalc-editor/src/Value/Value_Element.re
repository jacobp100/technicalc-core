open AST;
open Value_Builders;

let map = (element: foldState('a), i, i') =>
  switch (element) {
  | AST.(Superscript(_) | Label(_)) =>
    // Handled in Value.re
    assert(false)
  | Function({func, squareResultSuperscript}) =>
    let squareResultSuperscript =
      Belt.Option.map(squareResultSuperscript, superscriptBody);
    Value_Types.UnresolvedFunction(
      GenericFunction({func, squareResultSuperscript}),
      i,
      i',
    );
  | (Angle(_) | Base(_) | Conj | DecimalSeparator | Factorial | OpenBracket) as e
  | (Operator(_) | Percent | UnitConversion(_)) as e
  | (CloseBracket(_) | Digit(_) | Magnitude(_)) as e => Unresolved(e, i, i')
  | ImaginaryUnit(superscript) =>
    let superscript =
      Belt.Option.map(superscript, superscriptBody)
      ->Belt.Option.getWithDefault(Node.one);
    Resolved(Node.pow(Node.i, superscript));
  | NLog({base}) => UnresolvedFunction(NLog({base: base}), i, i')
  | Sum({start, end_}) => UnresolvedFunction(Sum({start, end_}), i, i')
  | Product({start, end_}) =>
    UnresolvedFunction(Product({start, end_}), i, i')
  | Rand(superscript) => Resolved(Node.rand->withSuperscript(superscript))
  | RandInt({a, b, superscript}) =>
    Resolved(Node.randInt(a, b)->withSuperscript(superscript))
  | NPR({n, r}) => Resolved(Node.nPr(n, r))
  | NCR({n, r}) => Resolved(Node.nCr(n, r))
  | Differential({x, body}) => Resolved(Node.differential(x, body))
  | Integral({a, b, body}) => Resolved(Node.integral(a, b, body))
  | Variable({nucleus, superscript}) =>
    Resolved(Node.variable(nucleus)->withSuperscript(superscript))
  | CustomAtom({value, superscript}) =>
    Resolved(Node.ofEncoded(value)->withSuperscript(superscript))
  | ConstPi(superscript) => Node.pi->withSuperscript(superscript)->Resolved
  | ConstE(superscript) => Node.e->withSuperscript(superscript)->Resolved
  | Frac({num, den, superscript}) =>
    Resolved(Node.div(num, den)->withSuperscript(superscript))
  | Min({a, b, superscript}) =>
    Resolved(Node.min(a, b)->withSuperscript(superscript))
  | Max({a, b, superscript}) =>
    Resolved(Node.max(a, b)->withSuperscript(superscript))
  | Gcd({a, b, superscript}) =>
    Resolved(Node.gcd(a, b)->withSuperscript(superscript))
  | Lcm({a, b, superscript}) =>
    Resolved(Node.lcm(a, b)->withSuperscript(superscript))
  | Abs({arg, superscript}) =>
    Resolved(Node.abs(arg)->withSuperscript(superscript))
  | Floor({arg, superscript}) =>
    Resolved(Node.floor(arg)->withSuperscript(superscript))
  | Ceil({arg, superscript}) =>
    Resolved(Node.ceil(arg)->withSuperscript(superscript))
  | Round({arg, superscript}) =>
    Resolved(Node.round(arg)->withSuperscript(superscript))
  | Sqrt({radicand, superscript}) =>
    Resolved(Node.sqrt(radicand)->withSuperscript(superscript))
  | NRoot({degree, radicand, superscript}) =>
    Node.pow(radicand, Node.div(Node.one, degree))
    ->withSuperscript(superscript)
    ->Resolved
  | Vector({elements, superscript}) =>
    Node.vector(elements)->withSuperscript(superscript)->Resolved
  | Table({elements, numRows, numColumns, superscript}) =>
    Node.matrix(numRows, numColumns, elements)
    ->withSuperscript(superscript)
    ->Resolved
  };
