open Formatting_Types
open Formatting_Util

let toString = (~format=defaultFormat, ~inline=false, a: Value_Types.t): string => {
  let body = switch a {
  | #...Scalar.t as aV => Formatting_Scalar.toString(~format, aV)
  | (#Matx(_) | #Vect(_)) as aV =>
    let matrix = switch aV {
    | #Matx(m) => m
    | #Vect(v) => Matrix.ofVector(v)
    }
    let tableFormat = switch format.mode {
    | Ascii | Unicode => Formatting_Matrix.formatAscii
    | Tex => Formatting_Matrix.formatTex
    | MathML => Formatting_Matrix.formatMathML
    }
    Formatting_Matrix.toString(~format, matrix, tableFormat)
  | #Pcnt(p) =>
    Formatting_Scalar.toString(~format, Scalar.Finite.toScalar(p)) ++ formatOperator(~format, "%")
  | #Mesr(m) => Formatting_Measure.toString(~format, m)
  }

  switch format.mode {
  | Ascii | Unicode | Tex => body
  | MathML =>
    let display = inline ? "inline" : "block"
    `<math xmlns="http://www.w3.org/1998/Math/MathML" display="${display}">${body}</math>`
  }
}
