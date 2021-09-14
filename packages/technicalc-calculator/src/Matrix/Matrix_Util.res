open Matrix_Types
open Matrix_Base

let scalarElements = (x: t): array<Scalar.t> => Obj.magic(x.elements)

let some = (x, fn) => Belt.Array.some(x.elements, fn)

let zipBy = (x: t, y: t, fn: (Scalar.t, Scalar.t) => Scalar.t): t => {
  if x.numRows == y.numRows && x.numColumns == y.numColumns {
    makeByIndexU(~numRows=x.numRows, ~numColumns=x.numColumns, (. i) => {
      fn(
        Belt.Array.getUnsafe(x.elements, i)->Scalar.Finite.toScalar,
        Belt.Array.getUnsafe(y.elements, i)->Scalar.Finite.toScalar,
      )
    })
  } else {
    empty
  }
}
