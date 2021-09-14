open Value_Core

%%private(
  let mapReal = (x: t, f: Real.t => Real.t) =>
    switch x {
    | #R(re) => ofReal(f(re))
    | _ => x
    }
)

type realBounds =
  | Real(Decimal.t, DecimalUtil.bounds)
  | Complex
  | NaN

%%private(
  let realBounds = (~lower=?, ~upper=?, x: t) =>
    switch x {
    | #Z => Real(Decimal.zero, DecimalUtil.bounds(~lower?, ~upper?, Decimal.zero))
    | #R(re) =>
      let decimal = Real.toDecimal(re)
      Real(decimal, DecimalUtil.bounds(~lower?, ~upper?, Real.toDecimal(re)))
    | #I(_)
    | #C(_, _) =>
      Complex
    | _ => NaN
    }
)

%%private(
  let mapRealDecimal = (x: t, f: Decimal.t => Decimal.t): t =>
    switch x {
    | #Z => ofDecimal(f(Decimal.zero))
    | #R(re) => Real.toDecimal(re)->f->ofDecimal
    | _ => nan
    }
)

let sin = (x: t): t =>
  switch x {
  | #Z => zero
  | #R(re) => Real.sin(re)->ofReal
  | #I(_)
  | #C(_) =>
    let iX = x * i
    i * (exp(-iX) - exp(iX)) / ofInt(2)
  | #V(_)
  | #M(_)
  | #P(_)
  | #N => nan
  }

let cosec = a => div(one, sin(a))

let asin = (a: t): t =>
  switch mapReal(a, Real.mod2Pi) {
  | #R(Rational(-1, 1, Unit)) => ofReal(Real.ofRational(-1, 2, Pi(1)))
  | #R(Rational(-1, 2, Sqrt(3))) => ofReal(Real.ofRational(-1, 3, Pi(1)))
  | #R(Rational(-1, 2, Sqrt(2))) => ofReal(Real.ofRational(-1, 4, Pi(1)))
  | #R(Rational(-1, 2, Unit)) => ofReal(Real.ofRational(-1, 6, Pi(1)))
  | #Z => zero
  | #R(Rational(1, 2, Unit)) => ofReal(Real.ofRational(1, 6, Pi(1)))
  | #R(Rational(1, 2, Sqrt(2))) => ofReal(Real.ofRational(1, 4, Pi(1)))
  | #R(Rational(1, 2, Sqrt(3))) => ofReal(Real.ofRational(1, 3, Pi(1)))
  | #R(Rational(1, 1, Unit)) => ofReal(Real.ofRational(1, 2, Pi(1)))
  | #R(_)
  | #I(_)
  | #C(_) =>
    switch realBounds(~lower=Decimal.minusOne, ~upper=Decimal.one, a) {
    | Real(_, BothBound | LowerBound | UpperBound | Inside) => mapRealDecimal(a, Decimal.asin)
    | Real(_, Outside)
    | Complex =>
      -i * log(i * a + sqrt(one - a * a))
    | NaN => nan
    }
  | #M(_)
  | #V(_)
  | #P(_)
  | #N => nan
  }

let sinh = (x: t): t =>
  switch x {
  | #Z => zero
  | #R(re) => Real.toDecimal(re)->Decimal.sinh->ofDecimal
  | #I(im) => i * ofReal(im)->sin
  | #C(_) => (exp(x) - exp(-x)) / ofInt(2)
  | _ => nan
  }

let asinh = (x: t): t =>
  switch x {
  | #Z => zero
  | #R(re) => Real.toDecimal(re)->Decimal.asinh->ofDecimal
  | #I(im) => i * ofReal(im)->asin
  | #C(_) => log(x + sqrt(x * x + one))
  | _ => nan
  }

let cos = (x: t): t =>
  switch x {
  | #Z => one
  | #R(re) => Real.cos(re)->ofReal
  | #I(_)
  | #C(_) =>
    let iX = x * i
    (exp(iX) + exp(-iX)) / ofInt(2)
  | #V(_)
  | #M(_)
  | #P(_)
  | #N => nan
  }

let sec = a => div(one, cos(a))

let acos = (a: t): t =>
  switch mapReal(a, Real.mod2Pi) {
  | #R(Rational(-1, 1, Unit)) => ofReal(Real.ofRational(1, 1, Pi(1)))
  | #R(Rational(-1, 2, Sqrt(3))) => ofReal(Real.ofRational(5, 6, Pi(1)))
  | #R(Rational(-1, 2, Sqrt(2))) => ofReal(Real.ofRational(3, 4, Pi(1)))
  | #R(Rational(-1, 2, Unit)) => ofReal(Real.ofRational(2, 3, Pi(1)))
  | #Z => ofReal(Real.ofRational(1, 2, Pi(1)))
  | #R(Rational(1, 2, Unit)) => ofReal(Real.ofRational(1, 3, Pi(1)))
  | #R(Rational(1, 2, Sqrt(2))) => ofReal(Real.ofRational(1, 4, Pi(1)))
  | #R(Rational(1, 2, Sqrt(3))) => ofReal(Real.ofRational(1, 6, Pi(1)))
  | #R(Rational(1, 1, Unit)) => zero
  | #R(_)
  | #I(_)
  | #C(_) =>
    switch realBounds(~lower=Decimal.minusOne, ~upper=Decimal.one, a) {
    | Real(_, BothBound | LowerBound | UpperBound | Inside) => mapRealDecimal(a, Decimal.acos)
    | Real(_, Outside)
    | Complex =>
      ofReal(Real.ofRational(1, 2, Pi(1))) - asin(a)
    | NaN => nan
    }
  | #M(_)
  | #V(_)
  | #P(_)
  | #N => nan
  }

let cosh = (x: t): t =>
  switch x {
  | #Z => one
  | #R(re) => Real.toDecimal(re)->Decimal.cosh->ofDecimal
  | #I(im) => ofReal(im)->cos
  | #C(_) => (exp(x) + exp(-x)) / ofInt(2)
  | _ => nan
  }

let acosh = (x: t): t =>
  switch realBounds(~lower=Decimal.one, x) {
  | Real(f, Inside) => Decimal.acosh(f)->ofDecimal
  | Real(_, LowerBound) => zero
  | Real(_, BothBound | UpperBound | Outside)
  /* acosh ix != i cosh x */
  | Complex =>
    /* From complex.js library */
    let res = acos(x)
    let imLteZero = switch res {
    | #Z
    | #R(_) => true
    | #I(im)
    | #C(_, im) =>
      Real.lte(im, Real.zero)
    | _ => false
    }
    if imLteZero {
      res * i
    } else {
      -res * i
    }
  | NaN => nan
  }

let tan = (x: t): t =>
  switch mapReal(x, Real.mod2Pi) {
  | #Z
  | #R(Rational(1 | 2, 1, Pi(1))) => zero
  | #R(Rational(1 | 5, 4, Pi(1))) => one
  | #R(Rational(3 | 7, 4, Pi(1))) => minusOne
  | #R(Rational(1 | 4, 3, Pi(1))) => ofReal(Real.ofRational(1, 1, Sqrt(3)))
  | #R(Rational(2 | 5, 3, Pi(1))) => ofReal(Real.ofRational(-1, 1, Sqrt(3)))
  | #R(Rational(1 | 7, 6, Pi(1))) => ofReal(Real.ofRational(1, 3, Sqrt(3)))
  | #R(Rational(5 | 11, 6, Pi(1))) => ofReal(Real.ofRational(-1, 3, Sqrt(3)))
  | #R(Rational(1 | 3, 2, Pi(1))) => nan
  | #R(_) => mapRealDecimal(x, Decimal.tan)
  | #I(_)
  | #C(_) =>
    let iX = x * i
    let a = exp(iX)
    let b = exp(-iX)
    (a - b) / ((a + b) * i)
  | #M(_)
  | #V(_)
  | #P(_)
  | #N => nan
  }

let cot = a => div(one, tan(a))

let atan = (a: t): t =>
  switch mapReal(a, Real.mod2Pi) {
  | #R(Rational(-1, 1, Sqrt(3))) => ofReal(Real.ofRational(-1, 3, Pi(1)))
  | #R(Rational(-1, 1, Unit)) => ofReal(Real.ofRational(-1, 4, Pi(1)))
  | #R(Rational(-1, 3, Sqrt(3))) => ofReal(Real.ofRational(-1, 6, Pi(1)))
  | #Z => zero
  | #R(Rational(1, 3, Sqrt(3))) => ofReal(Real.ofRational(1, 6, Pi(1)))
  | #R(Rational(1, 1, Unit)) => ofReal(Real.ofRational(1, 4, Pi(1)))
  | #R(Rational(1, 1, Sqrt(3))) => ofReal(Real.ofRational(1, 3, Pi(1)))
  | #R(_) => mapRealDecimal(a, Decimal.atan)
  | #I(Rational(1 | -1, 1, Unit)) => nan
  | (#I(_) | #C(_)) as vV =>
    let (re, im) = switch vV {
    | #I(im) => (Real.zero, im)
    | #C(re, im) => (re, im)
    }
    open Real
    let a = re
    let b = im
    let b' = {
      one - b
    }
    let d = {
      a * a + b' * b'
    }
    let two = Real.ofInt(2)
    let t1 = ofComplex((one - b * b - a * a) / d, -two * a / d)->log
    let (t1re, t1im) = switch t1 {
    | #Z => (Real.zero, Real.zero)
    | #R(re) => (re, Real.zero)
    | #I(im) => (Real.zero, im)
    | #C(re, im) => (re, im)
    | _ => (Real.nan, Real.nan)
    }
    open Real
    ofComplex(-t1im / two, t1re / two)
  | #M(_)
  | #V(_)
  | #P(_)
  | #N => nan
  }

let tanh = (x: t): t =>
  switch x {
  | #Z => zero
  | #R(re) => Real.toDecimal(re)->Decimal.tanh->ofDecimal
  | #I(im) => i * ofReal(im)->tan
  | #C(_) =>
    let a = exp(x)
    let b = exp(-x)
    (a - b) / (a + b)
  | _ => nan
  }

let atanh = (x: t): t =>
  switch realBounds(~lower=Decimal.minusOne, ~upper=Decimal.one, x) {
  | Real(f, Inside) => Decimal.atanh(f)->ofDecimal
  | Real(_, BothBound | LowerBound | UpperBound) => nan
  | Real(_, Outside)
  | Complex =>
    switch x {
    | #I(im) => i * ofReal(im)->atan
    | _ =>
      let two = ofInt(2)
      log((one + x) / (one - x)) / two
    }
  | NaN => nan
  }
