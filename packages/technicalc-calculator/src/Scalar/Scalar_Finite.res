type t = [#Zero | #Real(Real.t) | #Imag(Real.t) | #Cmpx(Real.t, Real.t)]

external toScalar: t => Scalar_Types.t = "%identity"

let ofScalar = (a: Scalar_Types.t): option<t> =>
  switch a {
  | #...t as finite => Some(finite)
  | #NaNN => None
  }
