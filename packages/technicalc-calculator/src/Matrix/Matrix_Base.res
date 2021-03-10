open Matrix_Types

let isEmpty = (a: t) => a.numRows == 0

let empty: t = {numRows: 0, numColumns: 0, elements: []}

let equal = (a: t, b: t) =>
  a.numRows == b.numRows &&
    (a.numColumns == b.numColumns &&
    Belt.Array.every2(a.elements, b.elements, Scalar.equal))

let make = (numRows, numColumns, elements) =>
  numRows * numColumns == Belt.Array.length(elements)
    ? {numRows: numRows, numColumns: numColumns, elements: elements}
    : empty

let makeByU = (numRows, numColumns, fn) => {
  numRows: numRows,
  numColumns: numColumns,
  elements: Belt.Array.makeByU(numRows * numColumns, (. i) => {
    let column = mod(i, numColumns)
    let row = i / numColumns
    fn(. row, column)
  }),
}

let identity = size => makeByU(size, size, (. a, b) => a == b ? Scalar.one : Scalar.zero)

let ofVector = elements => {
  numRows: Belt.Array.length(elements),
  numColumns: 1,
  elements: elements,
}

let some = (x, fn) => Belt.Array.some(x.elements, fn)

let getExn = (x, ~row, ~column) =>
  if row >= 0 && (row <= x.numRows && (column >= 0 && column <= x.numColumns)) {
    Belt.Array.getUnsafe(x.elements, column + row * x.numColumns)
  } else {
    raise(Not_found)
  }

let map = (x, fn) => {...x, elements: Belt.Array.map(x.elements, fn)}
let mapU = (x, fn) => {...x, elements: Belt.Array.mapU(x.elements, fn)}

let zipBy = (x, y, fn) =>
  x.numRows == y.numRows && x.numColumns == y.numColumns
    ? Some({...x, elements: Belt.Array.zipBy(x.elements, y.elements, fn)})
    : None
