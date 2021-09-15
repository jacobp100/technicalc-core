open Formatting_Types
open Formatting_Util

let toString = (~format=?, ~inline=false, a: Value_Types.t): string => {
  let body = switch a {
  | #...Scalar.t as aV => Formatting_Scalar.toString(~format?, aV)
  | (#Matx(_) | #Vect(_)) as aV =>
    let matrix = switch aV {
    | #Matx(m) => m
    | #Vect(v) => Matrix.ofVector(v)
    }
    let tableFormat = switch format {
    | None
    | Some({mode: String | Unicode}) => Formatting_Matrix.formatString
    | Some({mode: Tex}) => Formatting_Matrix.formatTex
    | Some({mode: MathML}) => Formatting_Matrix.formatMathML
    }
    Formatting_Matrix.toString(~format, matrix, tableFormat)
  | #Pcnt(p) =>
    Formatting_Scalar.toString(~format?, Scalar.Finite.toScalar(p)) ++ formatOperator("%", format)
  }

  switch format {
  | None
  | Some({mode: String | Unicode | Tex}) => body
  | Some({mode: MathML}) =>
    let display = inline ? "inline" : "block"
    `<math xmlns="http://www.w3.org/1998/Math/MathML" display="${display}">${body}</math>`
  }
}
