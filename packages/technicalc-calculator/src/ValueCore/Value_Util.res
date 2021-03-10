open Value_Base

%%private(let hundredS = ofInt(100))

let percentToNumerical = (x: Scalar.t) => Value_Arithmetic.div(ofScalar(x), hundredS)
