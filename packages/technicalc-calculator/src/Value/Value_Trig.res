open Value_Types
open Value_Base
open Value_Functions
open Value_Operators

%%private(
  let mapReal = (x: t, f: Real.t => Real.t) =>
    switch x {
    | #Real(re) => ofReal(f(re))
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
    | #Zero => Real(Decimal.zero, DecimalUtil.bounds(~lower?, ~upper?, Decimal.zero))
    | #Real(re) =>
      let decimal = Real.toDecimal(re)
      Real(decimal, DecimalUtil.bounds(~lower?, ~upper?, Real.toDecimal(re)))
    | #Imag(_)
    | #Cmpx(_, _) =>
      Complex
    | _ => NaN
    }
)

%%private(
  let mapRealDecimal = (x: t, f: Decimal.t => Decimal.t): t =>
    switch x {
    | #Zero => ofDecimal(f(Decimal.zero))
    | #Real(re) => Real.toDecimal(re)->f->ofDecimal
    | _ => nan
    }
)

let sin = (x: t): t =>
  switch x {
  | #Zero => zero
  | #Real(re) => Real.sin(re)->ofReal
  | #Imag(_)
  | #Cmpx(_) =>
    let iX = x * i
    i * (exp(-iX) - exp(iX)) / ofInt(2)
  | #Vect(_)
  | #Matx(_)
  | #Pcnt(_)
  | #NaNN => nan
  }

let cosec = a => sin(a)->inv

let asin = (a: t): t =>
  switch mapReal(a, Real.mod2Pi) {
  | #Real(Rational(-1, 1, Unit)) => ofReal(Real.ofRational(-1, 2, Pi(1)))
  | #Real(Rational(-1, 2, Sqrt(3))) => ofReal(Real.ofRational(-1, 3, Pi(1)))
  | #Real(Rational(-1, 2, Sqrt(2))) => ofReal(Real.ofRational(-1, 4, Pi(1)))
  | #Real(Rational(-1, 2, Unit)) => ofReal(Real.ofRational(-1, 6, Pi(1)))
  | #Zero => zero
  | #Real(Rational(1, 2, Unit)) => ofReal(Real.ofRational(1, 6, Pi(1)))
  | #Real(Rational(1, 2, Sqrt(2))) => ofReal(Real.ofRational(1, 4, Pi(1)))
  | #Real(Rational(1, 2, Sqrt(3))) => ofReal(Real.ofRational(1, 3, Pi(1)))
  | #Real(Rational(1, 1, Unit)) => ofReal(Real.ofRational(1, 2, Pi(1)))
  | #Real(_)
  | #Imag(_)
  | #Cmpx(_) =>
    switch realBounds(~lower=Decimal.minusOne, ~upper=Decimal.one, a) {
    | Real(_, BothBound | LowerBound | UpperBound | Inside) => mapRealDecimal(a, Decimal.asin)
    | Real(_, Outside)
    | Complex =>
      -i * log(i * a + sqrt(one - a * a))
    | NaN => nan
    }
  | #Matx(_)
  | #Vect(_)
  | #Pcnt(_)
  | #NaNN => nan
  }

let sinh = (x: t): t =>
  switch x {
  | #Zero => zero
  | #Real(re) => Real.toDecimal(re)->Decimal.sinh->ofDecimal
  | #Imag(im) => i * ofReal(im)->sin
  | #Cmpx(_) => (exp(x) - exp(-x)) / ofInt(2)
  | _ => nan
  }

let asinh = (x: t): t =>
  switch x {
  | #Zero => zero
  | #Real(re) => Real.toDecimal(re)->Decimal.asinh->ofDecimal
  | #Imag(im) => i * ofReal(im)->asin
  | #Cmpx(_) => log(x + sqrt(x * x + one))
  | _ => nan
  }

let cos = (x: t): t =>
  switch x {
  | #Zero => one
  | #Real(re) => Real.cos(re)->ofReal
  | #Imag(_)
  | #Cmpx(_) =>
    let iX = x * i
    (exp(iX) + exp(-iX)) / ofInt(2)
  | #Vect(_)
  | #Matx(_)
  | #Pcnt(_)
  | #NaNN => nan
  }

let sec = a => cos(a)->inv

let acos = (a: t): t =>
  switch mapReal(a, Real.mod2Pi) {
  | #Real(Rational(-1, 1, Unit)) => ofReal(Real.ofRational(1, 1, Pi(1)))
  | #Real(Rational(-1, 2, Sqrt(3))) => ofReal(Real.ofRational(5, 6, Pi(1)))
  | #Real(Rational(-1, 2, Sqrt(2))) => ofReal(Real.ofRational(3, 4, Pi(1)))
  | #Real(Rational(-1, 2, Unit)) => ofReal(Real.ofRational(2, 3, Pi(1)))
  | #Zero => ofReal(Real.ofRational(1, 2, Pi(1)))
  | #Real(Rational(1, 2, Unit)) => ofReal(Real.ofRational(1, 3, Pi(1)))
  | #Real(Rational(1, 2, Sqrt(2))) => ofReal(Real.ofRational(1, 4, Pi(1)))
  | #Real(Rational(1, 2, Sqrt(3))) => ofReal(Real.ofRational(1, 6, Pi(1)))
  | #Real(Rational(1, 1, Unit)) => zero
  | #Real(_)
  | #Imag(_)
  | #Cmpx(_) =>
    switch realBounds(~lower=Decimal.minusOne, ~upper=Decimal.one, a) {
    | Real(_, BothBound | LowerBound | UpperBound | Inside) => mapRealDecimal(a, Decimal.acos)
    | Real(_, Outside)
    | Complex =>
      ofReal(Real.ofRational(1, 2, Pi(1))) - asin(a)
    | NaN => nan
    }
  | #Matx(_)
  | #Vect(_)
  | #Pcnt(_)
  | #NaNN => nan
  }

let cosh = (x: t): t =>
  switch x {
  | #Zero => one
  | #Real(re) => Real.toDecimal(re)->Decimal.cosh->ofDecimal
  | #Imag(im) => ofReal(im)->cos
  | #Cmpx(_) => (exp(x) + exp(-x)) / ofInt(2)
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
    | #Zero
    | #Real(_) => true
    | #Imag(im)
    | #Cmpx(_, im) =>
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
  | #Zero
  | #Real(Rational(1 | 2, 1, Pi(1))) => zero
  | #Real(Rational(1 | 5, 4, Pi(1))) => one
  | #Real(Rational(3 | 7, 4, Pi(1))) => minusOne
  | #Real(Rational(1 | 4, 3, Pi(1))) => ofReal(Real.ofRational(1, 1, Sqrt(3)))
  | #Real(Rational(2 | 5, 3, Pi(1))) => ofReal(Real.ofRational(-1, 1, Sqrt(3)))
  | #Real(Rational(1 | 7, 6, Pi(1))) => ofReal(Real.ofRational(1, 3, Sqrt(3)))
  | #Real(Rational(5 | 11, 6, Pi(1))) => ofReal(Real.ofRational(-1, 3, Sqrt(3)))
  | #Real(Rational(1 | 3, 2, Pi(1))) => nan
  | #Real(_) => mapRealDecimal(x, Decimal.tan)
  | #Imag(_)
  | #Cmpx(_) =>
    let iX = x * i
    let a = exp(iX)
    let b = exp(-iX)
    (a - b) / ((a + b) * i)
  | #Matx(_)
  | #Vect(_)
  | #Pcnt(_)
  | #NaNN => nan
  }

let cot = a => tan(a)->inv

let atan = (a: t): t =>
  switch mapReal(a, Real.mod2Pi) {
  | #Real(Rational(-1, 1, Sqrt(3))) => ofReal(Real.ofRational(-1, 3, Pi(1)))
  | #Real(Rational(-1, 1, Unit)) => ofReal(Real.ofRational(-1, 4, Pi(1)))
  | #Real(Rational(-1, 3, Sqrt(3))) => ofReal(Real.ofRational(-1, 6, Pi(1)))
  | #Zero => zero
  | #Real(Rational(1, 3, Sqrt(3))) => ofReal(Real.ofRational(1, 6, Pi(1)))
  | #Real(Rational(1, 1, Unit)) => ofReal(Real.ofRational(1, 4, Pi(1)))
  | #Real(Rational(1, 1, Sqrt(3))) => ofReal(Real.ofRational(1, 3, Pi(1)))
  | #Real(_) => mapRealDecimal(a, Decimal.atan)
  | #Imag(Rational(1 | -1, 1, Unit)) => nan
  | (#Imag(_) | #Cmpx(_)) as vV =>
    open Real
    let (re, im) = switch vV {
    | #Imag(im) => (zero, im)
    | #Cmpx(re, im) => (re, im)
    }

    let a = re
    let b = im
    let b' = one - b
    let d = a * a + b' * b'
    let two = ofInt(2)
    let t1 = ofComplex((one - b * b - a * a) / d, -two * a / d)->log
    let (t1re, t1im) = switch t1 {
    | #Zero => (zero, zero)
    | #Real(re) => (re, zero)
    | #Imag(im) => (zero, im)
    | #Cmpx(re, im) => (re, im)
    | _ => (nan, nan)
    }

    ofComplex(-t1im / two, t1re / two)
  | #Matx(_)
  | #Vect(_)
  | #Pcnt(_)
  | #NaNN => nan
  }

let tanh = (x: t): t =>
  switch x {
  | #Zero => zero
  | #Real(re) => Real.toDecimal(re)->Decimal.tanh->ofDecimal
  | #Imag(im) => i * ofReal(im)->tan
  | #Cmpx(_) =>
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
    | #Imag(im) => i * ofReal(im)->atan
    | _ =>
      let two = ofInt(2)
      log((one + x) / (one - x)) / two
    }
  | NaN => nan
  }
