open Formatting_Types;
open Formatting_Util;

let toString = (~format=?, ~inline=false, a: Value_Types.t): string => {
  let body =
    switch (a) {
    | #Scalar.t as aV => Formatting_Scalar.toString(~format?, aV)
    | (`M(_) | `V(_)) as aV =>
      let matrix =
        switch (aV) {
        | `M(m) => m
        | `V(v) => Matrix.ofVector(v)
        };
      let tableFormat =
        switch (format) {
        | None
        | Some({mode: String | Unicode}) => Formatting_Matrix.formatString
        | Some({mode: Tex}) => Formatting_Matrix.formatTex
        | Some({mode: MathML}) => Formatting_Matrix.formatMathML
        };
      Formatting_Matrix.toString(~format, matrix, tableFormat);
    | `P(p) =>
      Formatting_Scalar.toString(~format?, p) ++ formatOperator("%", format)
    | `N => formatVariable("NaN", format)
    };

  switch (format) {
  | None
  | Some({mode: String | Unicode | Tex}) => body
  | Some({mode: MathML}) =>
    let display = inline ? "inline" : "block";
    "<math xmlns=\"http://www.w3.org/1998/Math/MathML\" display=\""
    ++ display
    ++ "\">"
    ++ body
    ++ "</math>";
  };
};
