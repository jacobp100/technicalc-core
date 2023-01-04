open AST
open Value_Builders

let map = (element: foldState<'a>, i, i') =>
  switch element {
  | AST.Fold_Placeholder(_) | Fold_CaptureGroupPlaceholder(_) =>
    // Handled in Value_Row.re
    assert false
  | (Fold_Add
  | Fold_Angle(_)
  | Fold_Base(_)
  | Fold_CloseBracket(_)
  | Fold_Conj
  | Fold_DecimalSeparator
  | Fold_Digit(_)
  | Fold_Div
  | Fold_Dot
  | Fold_Factorial
  | Fold_Magnitude(_)
  | Fold_Mul
  | Fold_OpenBracket
  | Fold_Percent
  | Fold_Sub
  | Fold_Transpose) as e
  | // Would normally be resolvable, but these have special semantics, so are handled separately
  (Fold_Frac(_)
  | Fold_ConstPi(_)
  | Fold_ConstE(_)
  | Fold_Constant(_)
  | Fold_ImaginaryUnit(_)
  | Fold_Variable(_)
  | Fold_X(_)) as e =>
    Value_Types.Unresolved(e, (i, i'))
  | Fold_Function({fn, resultSuperscript}) =>
    let resultSuperscript = Belt.Option.mapU(resultSuperscript, superscriptBodyU)
    UnresolvedFunction(GenericFunction({fn, resultSuperscript}), (i, i'))
  | Fold_NLog({base}) => UnresolvedFunction(NLog({base: base}), (i, i'))
  | Fold_Sum({from, to}) => UnresolvedFunction(Sum({from, to}), (i, i'))
  | Fold_Product({from, to}) => UnresolvedFunction(Product({from, to}), (i, i'))
  | Fold_Rand(superscript) => Resolved(withSuperscript(Rand, superscript), (i, i'))
  | Fold_RandInt({a, b, superscript}) =>
    Resolved(withSuperscript(RandInt(a, b), superscript), (i, i'))
  | Fold_NPR({n, r}) => Resolved(NPR(n, r), (i, i'))
  | Fold_NCR({n, r}) => Resolved(NCR(n, r), (i, i'))
  | Fold_Differential({at, body}) => Resolved(Differential({at, body}), (i, i'))
  | Fold_Integral({from, to, body}) => Resolved(Integral({from, to, body}), (i, i'))
  | Fold_Min({a, b, superscript}) => Resolved(withSuperscript(Min(a, b), superscript), (i, i'))
  | Fold_Max({a, b, superscript}) => Resolved(withSuperscript(Max(a, b), superscript), (i, i'))
  | Fold_Gcd({a, b, superscript}) => Resolved(withSuperscript(Gcd(a, b), superscript), (i, i'))
  | Fold_Lcm({a, b, superscript}) => Resolved(withSuperscript(Lcm(a, b), superscript), (i, i'))
  | Fold_Abs({arg, superscript}) => Resolved(withSuperscript(Abs(arg), superscript), (i, i'))
  | Fold_Floor({arg, superscript}) => Resolved(withSuperscript(Floor(arg), superscript), (i, i'))
  | Fold_Ceil({arg, superscript}) => Resolved(withSuperscript(Ceil(arg), superscript), (i, i'))
  | Fold_Round({arg, superscript}) => Resolved(withSuperscript(Round(arg), superscript), (i, i'))
  | Fold_Sqrt({radicand, superscript}) =>
    Resolved(withSuperscript(Sqrt(radicand), superscript), (i, i'))
  | Fold_NRoot({degree, radicand, superscript}) =>
    Resolved(withSuperscript(Pow(radicand, Div(One, degree)), superscript), (i, i'))
  | Fold_Table({elements, numColumns: 1, superscript}) =>
    Resolved(withSuperscript(Vector(elements), superscript), (i, i'))
  | Fold_Table({elements, numRows, numColumns, superscript}) =>
    Resolved(withSuperscript(Matrix({numRows, numColumns, elements}), superscript), (i, i'))
  }
