open Formatting_Types;

let formatOperator = (op, format) =>
  switch (format) {
  | Some({mode: MathML}) => "<mo>" ++ op ++ "</mo>"
  | _ => op
  };

let formatVariable = (var, format) =>
  switch (format) {
  | Some({mode: MathML}) => "<mi>" ++ var ++ "</mi>"
  | _ => var
  };
