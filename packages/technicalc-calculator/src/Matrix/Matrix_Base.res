open Matrix_Types

let isEmpty = (a: t) => a.numRows == 0

let empty: t = {numRows: 0, numColumns: 0, elements: []}

let eq = (a: t, b: t) =>
  a.numRows == b.numRows &&
  a.numColumns == b.numColumns &&
  Belt.Array.every2(a.elements, b.elements, (a, b) => {
    Scalar_Base.eq((a :> Scalar.t), (b :> Scalar.t))
  })

let make = (~numRows, ~numColumns, elements: array<Scalar.t>) =>
  numRows * numColumns == Belt.Array.length(elements) &&
    !Belt.Array.some(elements, Scalar_Base.isNaN)
    ? {numRows, numColumns, elements: Obj.magic(elements)}
    : empty

let makeByIndexU = (~numRows, ~numColumns, fn: (. int) => Scalar.t) => {
  let numElements = numRows * numColumns
  let elements = Belt.Array.makeUninitializedUnsafe(numElements)

  let rec iter = i =>
    if i < numElements {
      switch fn(. i)->Scalar.Finite.ofScalar {
      | Some(x) =>
        Belt.Array.setUnsafe(elements, i, x)
        iter(i + 1)
      | None => empty
      }
    } else {
      {
        numRows,
        numColumns,
        elements,
      }
    }

  iter(0)
}

let makeByU = (~numRows, ~numColumns, fn: (. int, int) => Scalar.t) =>
  makeByIndexU(~numRows, ~numColumns, (. i) => {
    let column = mod(i, numColumns)
    let row = i / numColumns
    fn(. row, column)
  })

let identity = (size: int) =>
  makeByU(~numRows=size, ~numColumns=size, (. a, b) => a == b ? Scalar_Base.one : Scalar_Base.zero)

let ofVector = elements => {
  numRows: Belt.Array.length(elements),
  numColumns: 1,
  elements,
}

@get external elements: t => array<Scalar.t> = "elements"

let getExn = (x, ~row, ~column) =>
  if row >= 0 && row < x.numRows && column >= 0 && column < x.numColumns {
    Belt.Array.getUnsafe(x.elements, column + row * x.numColumns)->Scalar_Finite.toScalar
  } else {
    assert false
  }

let mapU = (x: t, fn: (. Scalar.t) => Scalar.t): t =>
  makeByIndexU(~numRows=x.numRows, ~numColumns=x.numColumns, (. i) => {
    fn(. Belt.Array.getUnsafe(x.elements, i)->Scalar.Finite.toScalar)
  })
