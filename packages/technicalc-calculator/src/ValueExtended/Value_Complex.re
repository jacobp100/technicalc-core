open Value_Core;

let rec re = (a: t): t =>
  switch (a) {
  | `Z
  | `I(_) => `Z
  | `R(_) => a
  | `C(re, _) => ofReal(re)
  | `P(p) => re(Value_Util.percentToNumerical(p))
  | _ => `N
  };

let rec im = (a: t): t =>
  switch (a) {
  | `Z
  | `R(_) => `Z
  | `I(_) => a
  | `C(_, im) => ofImag(im)
  | `P(p) => im(Value_Util.percentToNumerical(p))
  | _ => `N
  };

let conj = (a: t): t =>
  switch (a) {
  | #Scalar.t as aS => Scalar.conj(aS)->ofScalar
  | `P(p) => Scalar.conj(p)->ofPercent
  | `V(elements) => `V(elements->Belt.Array.map(Scalar.conj))
  | `M(m) => `M(Matrix.map(m, Scalar.conj))
  | `N => `N
  };
