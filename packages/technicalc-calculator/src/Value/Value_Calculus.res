open Value_Types
open Value_Base
open Value_Operators

let differentiate = (f, x) => {
  let h = ofFloat(1e-4)
  let _2h = h * ofInt(2)
  let _8 = ofInt(8)

  let (pL1, pL2) = (f(x - h), f(x - _2h))
  let (pR1, pR2) = (f(x + h), f(x + _2h))

  let factors = pL2 - _8 * pL1 + _8 * pR1 - pR2
  factors / (ofInt(12) * h)
}

let integrate = (f: t => t, a: t, b: t): t => {
  open Decimal
  let (a, b) = (toDecimal(a), toDecimal(b))

  if b > a {
    let _2 = ofInt(2)
    let _4 = ofInt(4)
    let n = ofInt(100)
    let n2 = n * _2
    let h = (b - a) / (n * _2)

    let sum = ref(ofDecimal(a)->(f(_))->toDecimal + ofDecimal(b)->(f(_))->toDecimal)

    let i = ref(one)
    while i.contents < n2 && isFinite(sum.contents) {
      let v = ofDecimal(a + i.contents * h)
      sum := sum.contents + _4 * f(v)->toDecimal
      i := i.contents + _2
    }

    i := _2
    while i.contents < n2 - one && isFinite(sum.contents) {
      let v = ofDecimal(a + i.contents * h)
      sum := sum.contents + _2 * f(v)->toDecimal
      i := i.contents + _2
    }

    ofDecimal(sum.contents * h / ofInt(3))
  } else {
    Value_Base.nan
  }
}
