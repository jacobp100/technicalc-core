type percent = [#P(Scalar.t)]
type vector = [#V(Vector.t)]
type matrix = [#M(Matrix.t)]
type t = [Scalar.t | percent | vector | matrix | #N]
