open Formatting_Types

let formatOperator = (~format, op) =>
  switch format.mode {
  | MathML => `<mo>${op}</mo>`
  | _ => op
  }

let formatVariable = (~format, var) =>
  switch format.mode {
  | MathML => `<mi>${var}</mi>`
  | _ => var
  }
