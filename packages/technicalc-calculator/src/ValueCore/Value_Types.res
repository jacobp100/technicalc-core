type percent = [#P(Scalar.t)]
type matrix = [#M(Matrix.t)]
type vector = [#V(Vector.t)]
type t = [Scalar.t | percent | matrix | vector | #N]
