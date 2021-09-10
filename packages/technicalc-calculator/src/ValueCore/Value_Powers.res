open Value_Types
open Value_Base

%%private(let percentToNumerical = Value_Util.percentToNumerical)

let rec pow = (a: t, b: t): t =>
  switch (a, b) {
  // Base cases
  | (#N, _) | (_, #N) => #N

  // Percentages
  | (#P(aP), #P(bP)) => pow(percentToNumerical(aP), percentToNumerical(bP))
  | (#P(p), _) => pow(percentToNumerical(p), b)
  | (_, #P(p)) => pow(a, percentToNumerical(p))

  // Vectors
  | (#V(v), #R(Rational(2, 1, Unit))) => Vector.magnitudeSquared(v)->ofScalar
  | (#V(_), _)
  | (_, #V(_)) =>
    #N

  // Matrices
  | (#M(m), #R(Rational(-1, 1, Unit))) => Matrix.inverse(m)->ofMatrix
  | (#M(m), #Z) if m.numColumns == m.numRows => Matrix.identity(m.numRows)->ofMatrix
  | (#M(m), #R(Rational(gtZero, 1, Unit))) if gtZero >= 0 => Matrix.pow(m, gtZero)->ofMatrix
  | (#M(_), _)
  | (_, #M(_)) =>
    #N

  // Scalars
  | (#...Scalar.t as a, #...Scalar.t as b) => Scalar.pow(a, b)->ofScalar
  }
