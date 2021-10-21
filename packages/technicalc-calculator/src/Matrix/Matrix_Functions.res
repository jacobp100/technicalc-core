open Matrix_Types
open Matrix_Base

let determinant = (m: t): Scalar.t =>
  switch elements(m) {
  | [a, b, c, d] =>
    open Scalar_Operators
    a * d - b * c
  | [a, b, c, d, e, f, g, h, i] =>
    /* https://www.wolframalpha.com/input/?i=det(%7B%7Ba,b,c%7D,%7Bd,e,f%7D,%7Bg,h,i%7D%7D) */
    open Scalar_Operators
    a * e * i - a * f * h - b * d * i + b * f * g + c * d * h - c * e * g
  | _ => Scalar.nan
  }

let inverse = ({numRows, numColumns} as m: t): t =>
  switch elements(m) {
  | [a, b, c, d] =>
    open Scalar_Operators
    let factor = a * d - b * c
    let elements = [d / factor, -b / factor, -c / factor, a / factor]
    make(~numRows, ~numColumns, elements)
  | [a, b, c, d, e, f, g, h, i] =>
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
