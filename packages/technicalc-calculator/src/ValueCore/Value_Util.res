open Value_Base

%%private(let hundredS = ofInt(100))

let percentToNumerical = (x: Scalar.finite) =>
  Value_Arithmetic.div(Scalar.Finite.toScalar(x)->ofScalar, hundredS)
