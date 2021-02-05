open Value_Core;

let%private mapReal = (x: t, f: Real.t => Real.t) =>
  switch (x) {
  | `R(re) => `R(f(re))
  | _ => x
  };

type realBounds =
  | Real(Decimal.t, DecimalUtil.bounds)
  | Complex
  | NaN;

let%private realBounds = (~lower=?, ~upper=?, x: t) =>
  switch (x) {
  | `Z =>
    Real(Decimal.zero, DecimalUtil.bounds(~lower?, ~upper?, Decimal.zero))
  | `R(re) =>
    let decimal = Real.toDecimal(re);
    Real(decimal, DecimalUtil.bounds(~lower?, ~upper?, Real.toDecimal(re)));
  | `I(_)
  | `C(_, _) => Complex
  | _ => NaN
  };

let%private mapRealDecimal = (x: t, f: Decimal.t => Decimal.t): t =>
  switch (x) {
  | `Z => ofDecimal(f(Decimal.zero))
  | `R(re) => Real.toDecimal(re)->f->ofDecimal
  | _ => `N
  };

let sin = (x: t): t =>
  switch (x) {
  | `Z => zero
  | `R(re) => Real.sin(re)->ofReal
  | `I(_)
  | `C(_) =>
    let iX = x * i;
    i * (exp(- iX) - exp(iX)) / ofInt(2);
  | `V(_)
  | `M(_)
  | `P(_)
  | `N => `N
  };

let cosec = a => Value_Core.(div(one, sin(a)));

let asin = (a: t): t =>
  switch (mapReal(a, Real.mod2Pi)) {
  | `R(Rational((-1), 1, Unit)) => `R(Real.ofRational(-1, 2, Pi))
  | `R(Rational((-1), 2, Sqrt(3))) => `R(Real.ofRational(-1, 3, Pi))
  | `R(Rational((-1), 2, Sqrt(2))) => `R(Real.ofRational(-1, 4, Pi))
  | `R(Rational((-1), 2, Unit)) => `R(Real.ofRational(-1, 6, Pi))
  | `Z => zero
  | `R(Rational(1, 2, Unit)) => `R(Real.ofRational(1, 6, Pi))
  | `R(Rational(1, 2, Sqrt(2))) => `R(Real.ofRational(1, 4, Pi))
  | `R(Rational(1, 2, Sqrt(3))) => `R(Real.ofRational(1, 3, Pi))
  | `R(Rational(1, 1, Unit)) => `R(Real.ofRational(1, 2, Pi))
  | `R(_)
  | `I(_)
  | `C(_) =>
    switch (realBounds(~lower=Decimal.minusOne, ~upper=Decimal.one, a)) {
    | Real(_, BothBound | LowerBound | UpperBound | Inside) =>
      mapRealDecimal(a, Decimal.asin)
    | Real(_, Outside)
    | Complex => - i * log(i * a + sqrt(one - a * a))
    | NaN => `N
    }
  | `M(_)
  | `V(_)
  | `P(_)
  | `N => `N
  };

let sinh = (x: t): t =>
  switch (x) {
  | `Z => zero
  | `R(re) => Real.toDecimal(re)->Decimal.sinh->ofDecimal
  | `I(im) => i * ofReal(im)->sin
  | `C(_) => (exp(x) - exp(- x)) / ofInt(2)
  | _ => `N
  };

let asinh = (x: t): t =>
  switch (x) {
  | `Z => zero
  | `R(re) => Real.toDecimal(re)->Decimal.asinh->ofDecimal
  | `I(im) => i * ofReal(im)->asin
  | `C(_) => log(x + sqrt(x * x + one))
  | _ => `N
  };

let cos = (x: t): t =>
  switch (x) {
  | `Z => one
  | `R(re) => Real.cos(re)->ofReal
  | `I(_)
  | `C(_) =>
    let iX = x * i;
    (exp(iX) + exp(- iX)) / ofInt(2);
  | `V(_)
  | `M(_)
  | `P(_)
  | `N => `N
  };

let sec = a => Value_Core.(div(one, cos(a)));

let acos = (a: t): t =>
  switch (mapReal(a, Real.mod2Pi)) {
  | `R(Rational((-1), 1, Unit)) => `R(Real.ofRational(1, 1, Pi))
  | `R(Rational((-1), 2, Sqrt(3))) => `R(Real.ofRational(5, 6, Pi))
  | `R(Rational((-1), 2, Sqrt(2))) => `R(Real.ofRational(3, 4, Pi))
  | `R(Rational((-1), 2, Unit)) => `R(Real.ofRational(2, 3, Pi))
  | `Z => `R(Real.ofRational(1, 2, Pi))
  | `R(Rational(1, 2, Unit)) => `R(Real.ofRational(1, 3, Pi))
  | `R(Rational(1, 2, Sqrt(2))) => `R(Real.ofRational(1, 4, Pi))
  | `R(Rational(1, 2, Sqrt(3))) => `R(Real.ofRational(1, 6, Pi))
  | `R(Rational(1, 1, Unit)) => zero
  | `R(_)
  | `I(_)
  | `C(_) =>
    switch (realBounds(~lower=Decimal.minusOne, ~upper=Decimal.one, a)) {
    | Real(_, BothBound | LowerBound | UpperBound | Inside) =>
      mapRealDecimal(a, Decimal.acos)
    | Real(_, Outside)
    | Complex => ofReal(Real.ofRational(1, 2, Pi)) - asin(a)
    | NaN => `N
    }
  | `M(_)
  | `V(_)
  | `P(_)
  | `N => `N
  };

let cosh = (x: t): t =>
  switch (x) {
  | `Z => one
  | `R(re) => Real.toDecimal(re)->Decimal.cosh->ofDecimal
  | `I(im) => ofReal(im)->cos
  | `C(_) => (exp(x) + exp(- x)) / ofInt(2)
  | _ => `N
  };

let acosh = (x: t): t =>
  switch (realBounds(~lower=Decimal.one, x)) {
  | Real(f, Inside) => Decimal.acosh(f)->ofDecimal
  | Real(_, LowerBound) => zero
  | Real(_, BothBound | UpperBound | Outside)
  /* acosh ix != i cosh x */
  | Complex =>
    /* From complex.js library */
    let res = acos(x);
    let imLteZero =
      switch (res) {
      | `Z
      | `R(_) => true
      | `I(im)
      | `C(_, im) => Real.(im <= zero)
      | _ => false
      };
    if (imLteZero) {
      res * i;
    } else {
      - res * i;
    };
  | NaN => `N
  };

let tan = (x: t): t =>
  switch (mapReal(x, Real.mod2Pi)) {
  | `Z
  | `R(Rational(1 | 2, 1, Pi)) => zero
  | `R(Rational(1 | 5, 4, Pi)) => one
  | `R(Rational(3 | 7, 4, Pi)) => minusOne
  | `R(Rational(1 | 4, 3, Pi)) => `R(Real.ofRational(1, 1, Sqrt(3)))
  | `R(Rational(2 | 5, 3, Pi)) => `R(Real.ofRational(-1, 1, Sqrt(3)))
  | `R(Rational(1 | 7, 6, Pi)) => `R(Real.ofRational(1, 3, Sqrt(3)))
  | `R(Rational(5 | 11, 6, Pi)) => `R(Real.ofRational(-1, 3, Sqrt(3)))
  | `R(Rational(1 | 3, 2, Pi)) => `N
  | `R(_) => mapRealDecimal(x, Decimal.tan)
  | `I(_)
  | `C(_) =>
    let iX = x * i;
    let a = exp(iX);
    let b = exp(- iX);
    (a - b) / ((a + b) * i);
  | `M(_)
  | `V(_)
  | `P(_)
  | `N => `N
  };

let cot = a => Value_Core.(div(one, tan(a)));

let atan = (a: t): t =>
  switch (mapReal(a, Real.mod2Pi)) {
  | `R(Rational((-1), 1, Sqrt(3))) => `R(Real.ofRational(-1, 3, Pi))
  | `R(Rational((-1), 1, Unit)) => `R(Real.ofRational(-1, 4, Pi))
  | `R(Rational((-1), 3, Sqrt(3))) => `R(Real.ofRational(-1, 6, Pi))
  | `Z => zero
  | `R(Rational(1, 3, Sqrt(3))) => `R(Real.ofRational(1, 6, Pi))
  | `R(Rational(1, 1, Unit)) => `R(Real.ofRational(1, 4, Pi))
  | `R(Rational(1, 1, Sqrt(3))) => `R(Real.ofRational(1, 3, Pi))
  | `R(_) => mapRealDecimal(a, Decimal.atan)
  | `I(Rational(1 | (-1), 1, Unit)) => `N
  | (`I(_) | `C(_)) as vV =>
    let (re, im) =
      switch (vV) {
      | `I(im) => (Real.zero, im)
      | `C(re, im) => (re, im)
      };
    let a = re;
    let b = im;
    let b' = Real.(one - b);
    let d = Real.(a * a + b' * b');
    let two = Real.ofInt(2);
    let t1 =
      ofComplex(Real.((one - b * b - a * a) / d), Real.(- two * a / d))->log;
    let (t1re, t1im) =
      switch (t1) {
      | `Z => (Real.zero, Real.zero)
      | `R(re) => (re, Real.zero)
      | `I(im) => (Real.zero, im)
      | `C(re, im) => (re, im)
      | _ => (Real.nan, Real.nan)
      };
    ofComplex(Real.(- t1im / two), Real.(t1re / two));
  | `M(_)
  | `V(_)
  | `P(_)
  | `N => `N
  };

let tanh = (x: t): t =>
  switch (x) {
  | `Z => zero
  | `R(re) => Real.toDecimal(re)->Decimal.tanh->ofDecimal
  | `I(im) => i * ofReal(im)->tan
  | `C(_) =>
    let a = exp(x);
    let b = exp(- x);
    (a - b) / (a + b);
  | _ => `N
  };

let atanh = (x: t): t =>
  switch (realBounds(~lower=Decimal.minusOne, ~upper=Decimal.one, x)) {
  | Real(f, Inside) => Decimal.atanh(f)->ofDecimal
  | Real(_, BothBound | LowerBound | UpperBound) => `N
  | Real(_, Outside)
  | Complex =>
    switch (x) {
    | `I(im) => i * ofReal(im)->atan
    | _ =>
      let two = ofInt(2);
      log((one + x) / (one - x)) / two;
    }
  | NaN => `N
  };
