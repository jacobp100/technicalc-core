%%private(let hundredS = Scalar.ofInt(100))

let percentToNumerical = (x: Scalar.Finite.t): Scalar.t =>
  Scalar.div(Scalar.Finite.toScalar(x), hundredS)
