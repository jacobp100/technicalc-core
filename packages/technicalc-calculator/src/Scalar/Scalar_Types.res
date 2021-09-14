type finite = [#Z | #R(Real.t) | #I(Real.t) | #C(Real.t, Real.t)]
type t = [finite | #N]
