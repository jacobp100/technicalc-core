open Matrix_Types
open Matrix_Base

%%private(let squareElements = m => m.numRows == m.numColumns ? Some(elements(m)) : None)

let determinant = (m: t): Scalar.t =>
  switch squareElements(m) {
  | Some([a, b, c, d]) =>
    open Scalar_Operators
    a * d - b * c
  | Some([a, b, c, d, e, f, g, h, i]) =>
    /* https://www.wolframalpha.com/input/?i=det(%7B%7Ba,b,c%7D,%7Bd,e,f%7D,%7Bg,h,i%7D%7D) */
    open Scalar_Operators
    a * e * i - a * f * h - b * d * i + b * f * g + c * d * h - c * e * g
  | _ => Scalar.nan
  }
let trace = ({numRows, numColumns} as m) =>
  if numRows == numColumns {
    let out = ref(Scalar.zero)
    for i in 0 to numRows - 1 {
      out := Scalar.add(out.contents, getExn(m, ~row=i, ~column=i))
    }
    out.contents
  } else {
    Scalar.nan
  }

let transpose = (m: t) =>
  makeByU(~numRows=m.numColumns, ~numColumns=m.numRows, (. row, column) => {
    getExn(m, ~row=column, ~column=row)
  })

let inv = ({numRows, numColumns} as m: t): t =>
  switch squareElements(m) {
  | Some([a, b, c, d]) =>
    open Scalar_Operators
    let factor = a * d - b * c
    let elements = [d / factor, -b / factor, -c / factor, a / factor]
    make(~numRows, ~numColumns, elements)
  | Some([a, b, c, d, e, f, g, h, i]) =>
    /* https://www.wolframalpha.com/input/?i=%7B%7Ba,b,c%7D,%7Bd,e,f%7D,%7Bg,h,i%7D%7D%5E-1 */
    open Scalar_Operators
    let factor = a * e * i - a * f * h - b * d * i + b * f * g + c * d * h - c * e * g
    let elements = [
      (e * i - f * h) / factor,
      (c * h - b * i) / factor,
      (b * f - c * e) / factor,
      (f * g - d * i) / factor,
      (a * i - c * g) / factor,
      (c * d - a * f) / factor,
      (d * h - e * g) / factor,
      (b * g - a * h) / factor,
      (a * e - b * d) / factor,
    ]
    make(~numRows, ~numColumns, elements)
  | _ => empty
  }

%%private(
  let swapRows = ({numRows, numColumns} as m: t, i: int, j: int) =>
    makeByU(~numRows, ~numColumns, (. row, column) => {
      let row = if row == i {
        j
      } else if row == j {
        i
      } else {
        row
      }
      getExn(m, ~row, ~column)
    })
)

%%private(
  let divideRow = ({numRows, numColumns} as m: t, i: int, by: Scalar.t) =>
    makeByU(~numRows, ~numColumns, (. row, column) => {
      let current = getExn(m, ~row, ~column)
      row == i ? Scalar.div(current, by) : current
    })
)

%%private(
  let subtractRow = ({numRows, numColumns} as m: t, ~from, ~to, ~factor) =>
    makeByU(~numRows, ~numColumns, (. row, column) => {
      let current = getExn(m, ~row, ~column)
      if row == to {
        Scalar.sub(current, Scalar.mul(getExn(m, ~row=from, ~column), factor))
      } else {
        current
      }
    })
)

// https://en.wikipedia.org/wiki/Row_echelon_form#Pseudocode_for_reduced_row_echelon_form
let rref = (m: t) => {
  let {numRows, numColumns} = m
  let out = ref(m)
  let lead = ref(0)
  let r = ref(0)

  while r.contents < numRows && lead.contents < numColumns {
    let i = ref(r.contents)

    while (
      lead.contents < numColumns &&
        getExn(out.contents, ~row=i.contents, ~column=lead.contents)->Scalar.eq(Scalar.zero)
    ) {
      i := i.contents + 1

      if i.contents == numRows {
        i := r.contents
        lead := lead.contents + 1
      }
    }

    if lead.contents < numColumns {
      if i.contents != r.contents {
        out := swapRows(out.contents, i.contents, r.contents)
      }

      out :=
        divideRow(
          out.contents,
          r.contents,
          getExn(out.contents, ~row=r.contents, ~column=lead.contents),
        )

      for j in 0 to numRows - 1 {
        if j != r.contents {
          let factor = getExn(out.contents, ~row=j, ~column=lead.contents)
          out := subtractRow(out.contents, ~from=r.contents, ~to=j, ~factor)
        }
      }

      lead := lead.contents + 1
    }

    r := r.contents + 1
  }

  out.contents
}
