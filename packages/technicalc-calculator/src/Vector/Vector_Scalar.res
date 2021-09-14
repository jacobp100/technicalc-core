open Vector_Types
open Vector_Base

let mulScalar = (m: t, s: Scalar.t) => map(m, Scalar.mul(_, s))
let divScalar = (m: t, s: Scalar.t) => map(m, Scalar.div(_, s))
