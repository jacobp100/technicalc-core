open AST
open Value_Builders

let map = (element: foldState<'a>, i, i') =>
  switch element {
  | AST.Fold_Placeholder(_) | Fold_CaptureGroupPlaceholder(_) =>
    // Handled in Value.re
    assert false
  | (Fold_Angle(_)
    | Fold_Base(_)
    | Fold_Conj
    | Fold_DecimalSeparator
    | Fold_Factorial
    | Fold_OpenBracket
    | Fold_Operator(_)
    | Fold_Percent
    | Fold_UnitConversion(_)
    | Fold_CloseBracket(_)
    | Fold_Digit(_)
    | Fold_Magnitude(_)) as e =>
    Value_Types.Unresolved(e, i, i')
  | Fold_Function({fn, resultSuperscript}) =>
    let resultSuperscript = Belt.Option.map(resultSuperscript, superscriptBody)
    UnresolvedFunction(GenericFunction({fn, resultSuperscript}), i, i')
  | Fold_NLog({base}) => UnresolvedFunction(NLog({base: base}), i, i')
  | Fold_Sum({from, to}) => UnresolvedFunction(Sum({from, to}), i, i')
  | Fold_Product({from, to}) => UnresolvedFunction(Product({from, to}), i, i')
  | Fold_ImaginaryUnit(superscript) => withSuperscript(I, superscript)->Resolved
  | Fold_Rand(superscript) => withSuperscript(Rand, superscript)->Resolved
  | Fold_RandInt({a, b, superscript}) => withSuperscript(RandInt(a, b), superscript)->Resolved
  | Fold_NPR({n, r}) => NPR(n, r)->Resolved
  | Fold_NCR({n, r}) => NCR(n, r)->Resolved
  | Fold_Differential({at, body}) => Differential({at, body})->Resolved
  | Fold_Integral({from, to, body}) => Integral({from, to, body})->Resolved
  | Fold_X(superscript) => withSuperscript(X, superscript)->Resolved
  | Fold_Variable({id, superscript}) => withSuperscript(Variable(id), superscript)->Resolved
  | Fold_CustomAtom({value, superscript}) =>
    withSuperscript(OfEncoded(value), superscript)->Resolved
  | Fold_ConstPi(superscript) => withSuperscript(Pi, superscript)->Resolved
  | Fold_ConstE(superscript) => withSuperscript(E, superscript)->Resolved
  | Fold_Frac({num, den, superscript}) => withSuperscript(Div(num, den), superscript)->Resolved
  | Fold_MFrac({integer, num, den, superscript}) =>
    withSuperscript(Add(integer, Div(num, den)), superscript)->Resolved
  | Fold_Min({a, b, superscript}) => withSuperscript(Min(a, b), superscript)->Resolved
  | Fold_Max({a, b, superscript}) => withSuperscript(Max(a, b), superscript)->Resolved
  | Fold_Gcd({a, b, superscript}) => withSuperscript(Gcd(a, b), superscript)->Resolved
  | Fold_Lcm({a, b, superscript}) => withSuperscript(Lcm(a, b), superscript)->Resolved
  | Fold_Abs({arg, superscript}) => withSuperscript(Abs(arg), superscript)->Resolved
  | Fold_Floor({arg, superscript}) => withSuperscript(Floor(arg), superscript)->Resolved
  | Fold_Ceil({arg, superscript}) => withSuperscript(Ceil(arg), superscript)->Resolved
  | Fold_Round({arg, superscript}) => withSuperscript(Round(arg), superscript)->Resolved
  | Fold_Sqrt({radicand, superscript}) => withSuperscript(Sqrt(radicand), superscript)->Resolved
  | Fold_NRoot({degree, radicand, superscript}) =>
    withSuperscript(Pow(radicand, Div(One, degree)), superscript)->Resolved
  | Fold_Table({elements, numColumns: 1, superscript}) =>
    withSuperscript(Vector(elements), superscript)->Resolved
  | Fold_Table({elements, numRows, numColumns, superscript}) =>
    withSuperscript(Matrix({numRows, numColumns, elements}), superscript)->Resolved
  }
