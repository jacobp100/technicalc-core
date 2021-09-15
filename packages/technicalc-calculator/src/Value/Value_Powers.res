open Value_Types
open Value_Base

%%private(let percentToNumerical = Value_Util.percentToNumerical)

let pow = (a: t, b: t): t =>
  switch (a, b) {
  // Percentages
  | (#Pcnt(aP), #Pcnt(bP)) => Scalar.pow(percentToNumerical(aP), percentToNumerical(bP))->ofScalar
  | (#Pcnt(p), #...Scalar.t as b) => Scalar.pow(percentToNumerical(p), b)->ofScalar
  | (#...Scalar.t as a, #Pcnt(p)) => Scalar.pow(a, percentToNumerical(p))->ofScalar

  // Vectors
  | (#Vect(v), #Real(Rational(2, 1, Unit))) => Vector.magnitudeSquared(v)->ofScalar
  | (#Vect(_), _)
  | (_, #Vect(_)) => nan

  // Matrices
  | (#Matx(m), #Real(Rational(-1, 1, Unit))) => Matrix.inverse(m)->ofMatrix
  | (#Matx(m), #Zero) if m.numColumns == m.numRows => Matrix.identity(m.numRows)->ofMatrix
  | (#Matx(m), #Real(Rational(gtZero, 1, Unit))) if gtZero >= 0 => Matrix.pow(m, gtZero)->ofMatrix
  | (#Matx(_), _)
  | (_, #Matx(_)) => nan

  // Scalars
  | (#...Scalar.t as a, #...Scalar.t as b) => Scalar.pow(a, b)->ofScalar
  }
