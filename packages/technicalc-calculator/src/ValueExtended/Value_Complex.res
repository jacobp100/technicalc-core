open Value_Core

let rec re = (a: t): t =>
  switch a {
  | #Z
  | #I(_) => zero
  | #R(_) => a
  | #C(re, _) => ofReal(re)
  | #P(p) => re(Value_Util.percentToNumerical(p))
  | _ => nan
  }

let rec im = (a: t): t =>
  switch a {
  | #Z
  | #R(_) => zero
  | #I(_) => a
  | #C(_, im) => ofReal(im)
  | #P(p) => im(Value_Util.percentToNumerical(p))
  | _ => nan
  }

let conj = (a: t): t =>
  switch a {
  | #...Scalar.t as aS => Scalar.conj(aS)->ofScalar
  | #P(p) =>
    let p = Scalar.Finite.toScalar(p)
    Scalar.conj(p)->ofPercent
  | #V(v) => ofVector(Vector.map(v, Scalar.conj))
  | #M(m) => ofMatrix(Matrix.map(m, Scalar.conj))
  }
