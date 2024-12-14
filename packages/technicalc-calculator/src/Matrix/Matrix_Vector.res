open Matrix_Types
open Matrix_Base

let mulVector = (m: t, v: Vector.t): Vector.t => {
  let size = Vector.size(v)

  if m.numColumns == size {
    Vector.makeBy(m.numRows, row => {
      let element = ref(Scalar.zero)
      for i in 0 to m.numColumns - 1 {
        let elementProduct = Scalar.mul(getExn(m, ~row, ~column=i), Vector.getExn(v, i))
        element := Scalar.add(element.contents, elementProduct)
      }
      element.contents
    })
  } else {
    Vector.empty
  }
}

let preMulVector = (v: Vector.t, m: t): t => {
  let size = Vector.size(v)

  if m.numRows == 1 && m.numColumns == size {
    makeBy(~numRows=size, ~numColumns=size, (row, column) => {
      Scalar.mul(Vector.getExn(v, row), getExn(m, ~row=0, ~column))
    })
  } else {
    empty
  }
}
