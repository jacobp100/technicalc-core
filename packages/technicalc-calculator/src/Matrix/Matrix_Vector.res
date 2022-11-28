open Matrix_Types
open Matrix_Base

let mulVector = (m: t, v: Vector.t): Vector.t => {
  let size = Vector.size(v)

  if m.numColumns == size {
    Vector.makeByU(m.numRows, (. row) => {
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
