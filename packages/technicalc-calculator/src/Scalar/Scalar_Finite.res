open Scalar_Types

external toScalar: finite => t = "%identity"

let ofScalar = (a: t): option<finite> =>
  switch a {
  | #...finite as finite => Some(finite)
  | #N => None
  }
