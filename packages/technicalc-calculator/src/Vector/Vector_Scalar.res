open Vector_Types
open Vector_Base

let mulScalar = (m: t, s: Scalar.t) => map(m, x => Scalar.mul(x, s))
let divScalar = (m: t, s: Scalar.t) => map(m, x => Scalar.div(x, s))
