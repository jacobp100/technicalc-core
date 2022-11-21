open Matrix_Types
open Matrix_Base

let neg = (m: t) => mapU(m, (. x) => Scalar.neg(x))

%%private(
  let zipByU = (a: t, b: t, fn: (. Scalar.t, Scalar.t) => Scalar.t): t =>
    if a.numRows == b.numRows && a.numColumns == b.numColumns {
      let aElements = elements(a)
      let bElements = elements(b)

      makeByIndexU(~numRows=a.numRows, ~numColumns=a.numColumns, (. i) => {
        fn(. Belt.Array.getUnsafe(aElements, i), Belt.Array.getUnsafe(bElements, i))
      })
    } else {
      empty
    }
)

let add = (a: t, b: t): t => zipByU(a, b, (. a, b) => Scalar.add(a, b))

let sub = (a: t, b: t): t => zipByU(a, b, (. a, b) => Scalar.sub(a, b))

let mul = (a: t, b: t): t =>
  if a.numColumns == b.numRows {
    makeByU(~numRows=a.numRows, ~numColumns=b.numColumns, (. row, column) => {
      let element = ref(Scalar.zero)
      for i in 0 to a.numColumns - 1 {
        let elementProduct = Scalar.mul(getExn(a, ~row, ~column=i), getExn(b, ~row=i, ~column))
        element := Scalar.add(element.contents, elementProduct)
      }
      element.contents
    })
  } else {
    empty
  }

let powInt = (m: t, i: int): t =>
  if i >= 0 && m.numColumns == m.numRows {
    let x = ref(identity(m.numRows))
    for _ in 0 to i - 1 {
      x := mul(x.contents, m)
    }
    x.contents
  } else {
    empty
  }
