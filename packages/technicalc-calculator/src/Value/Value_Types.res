type percent = [#Pcnt(Scalar.Finite.t)]
type vector = [#Vect(Vector.t)]
type matrix = [#Matx(Matrix.t)]
type t = [Scalar.t | percent | vector | matrix]
