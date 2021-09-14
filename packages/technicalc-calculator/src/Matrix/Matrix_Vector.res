open Matrix_Types
open Matrix_Base

let mulVector = (m: t, v: Vector.t): Vector.t => {
  let size = Vector.size(v)

  if m.numColumns == size {
    Vector.makeByU(size, (. row) => {
      let element = ref(Scalar.zero)
      for column in 0 to size - 1 {
        let elementProduct = Scalar.mul(
          getExn(~row, ~column, m)->Scalar.Finite.toScalar,
          Vector.getExn(v, column)->Scalar.Finite.toScalar,
        )
        element := Scalar.add(element.contents, elementProduct)
      }
      element.contents
    })
  } else {
    Vector.empty
  }
}
