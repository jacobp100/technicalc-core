open Matrix_Types
open Matrix_Base
open Matrix_Util

let add = (a: t, b: t): t => zipBy(a, b, Scalar.add)

let sub = (a: t, b: t): t => zipBy(a, b, Scalar.sub)

let mul = (a: t, b: t): t =>
  if a.numColumns == b.numColumns {
    let shape = a.numColumns
    makeByU(~numRows=shape, ~numColumns=shape, (. row, column) => {
      let element = ref(Scalar.zero)
      for i in 0 to shape - 1 {
        let elementProduct = Scalar.mul(
          getExn(a, ~row, ~column=i)->Scalar.Finite.toScalar,
          getExn(b, ~row=i, ~column)->Scalar.Finite.toScalar,
        )
        element := Scalar.add(element.contents, elementProduct)
      }
      element.contents
    })
  } else {
    empty
  }
