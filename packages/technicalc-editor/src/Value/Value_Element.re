open AST;
open Value_Builders;

let map = (element: foldState('a), i, i') =>
  switch (element) {
  | AST.(Superscript(_) | Label(_)) =>
    // Handled in Value.re
    assert(false)
  | (Angle(_) | Base(_) | Conj | DecimalSeparator | Factorial | OpenBracket) as e
  | (Operator(_) | Percent | UnitConversion(_)) as e
  | (CloseBracket(_) | Digit(_) | Magnitude(_)) as e =>
    Value_Types.Unresolved(e, i, i')
  | Function({func, resultSuperscript}) =>
    let resultSuperscript =
      Belt.Option.map(resultSuperscript, superscriptBody);
    UnresolvedFunction(GenericFunction({func, resultSuperscript}), i, i');
  | NLog({base}) => UnresolvedFunction(NLog({base: base}), i, i')
  | Sum({from, to_}) => UnresolvedFunction(Sum({from, to_}), i, i')
  | Product({from, to_}) => UnresolvedFunction(Product({from, to_}), i, i')
  | ImaginaryUnit(superscript) => withSuperscript(I, superscript)->Resolved
  | Rand(superscript) => withSuperscript(Rand, superscript)->Resolved
  | RandInt({a, b, superscript}) =>
    withSuperscript(RandInt(a, b), superscript)->Resolved
  | NPR({n, r}) => NPR(n, r)->Resolved
  | NCR({n, r}) => NCR(n, r)->Resolved
  | Differential({at, body}) => Differential({at, body})->Resolved
  | Integral({from, to_, body}) => Integral({from, to_, body})->Resolved
  | Variable({nucleus, superscript}) =>
    withSuperscript(Variable(nucleus), superscript)->Resolved
  | CustomAtom({value, superscript}) =>
    withSuperscript(OfEncoded(value), superscript)->Resolved
  | ConstPi(superscript) => withSuperscript(Pi, superscript)->Resolved
  | ConstE(superscript) => withSuperscript(E, superscript)->Resolved
  | Frac({num, den, superscript}) =>
    withSuperscript(Div(num, den), superscript)->Resolved
  | MFrac({integer, num, den, superscript}) =>
    withSuperscript(Add(integer, Div(num, den)), superscript)->Resolved
  | Min({a, b, superscript}) =>
    withSuperscript(Min(a, b), superscript)->Resolved
  | Max({a, b, superscript}) =>
    withSuperscript(Max(a, b), superscript)->Resolved
  | Gcd({a, b, superscript}) =>
    withSuperscript(Gcd(a, b), superscript)->Resolved
  | Lcm({a, b, superscript}) =>
    withSuperscript(Lcm(a, b), superscript)->Resolved
  | Abs({arg, superscript}) =>
    withSuperscript(Abs(arg), superscript)->Resolved
  | Floor({arg, superscript}) =>
    withSuperscript(Floor(arg), superscript)->Resolved
  | Ceil({arg, superscript}) =>
    withSuperscript(Ceil(arg), superscript)->Resolved
  | Round({arg, superscript}) =>
    withSuperscript(Round(arg), superscript)->Resolved
  | Sqrt({radicand, superscript}) =>
    withSuperscript(Sqrt(radicand), superscript)->Resolved
  | NRoot({degree, radicand, superscript}) =>
    withSuperscript(Pow(radicand, Div(One, degree)), superscript)->Resolved
  | Vector({elements, superscript}) =>
    withSuperscript(Vector(elements), superscript)->Resolved
  | Table({elements, numRows, numColumns, superscript}) =>
    withSuperscript(Matrix({numRows, numColumns, elements}), superscript)
    ->Resolved
  };
