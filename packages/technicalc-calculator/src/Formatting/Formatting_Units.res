open Formatting_Types
open Units_Types

let formatPrefix = (~mode, prefix: prefix) =>
  switch prefix {
  | Micro =>
    switch mode {
    | MathML => "&#x3BC;"
    | Tex => `\\micro`
    | Ascii => "u"
    | Unicode => Formatting_Unicode.mu
    }
  | _ => Belt.Array.getExn(Formatting_Units_Eval.prefixes, Obj.magic(prefix))
  }

%%private(
  let ohm = (~mode) => {
    switch mode {
    | MathML => "&#x3A9;"
    | Tex => `\\omega`
    | Ascii => "ohm"
    | Unicode => Formatting_Unicode.omegaUpper
    }
  }
)

%%private(
  let deg = (~mode) =>
    switch mode {
    | MathML => "&#x00B0;"
    | Tex => `{}^{\\circ}`
    | Ascii => "deg "
    | Unicode => Formatting_Unicode.degree
    }
)

let formatName = (~mode, unit: name) =>
  switch unit {
  | Ohm => ohm(~mode)
  | Celsius => deg(~mode) ++ "C"
  | Fahrenheit => deg(~mode) ++ "F"
  | _ => Belt.Array.getExn(Formatting_Units_Eval.prefixes, Obj.magic(unit))
  }

%%private(
  let formatUnit = (~mode, ~prefix: prefix, ~name: name) => {
    let body = formatPrefix(~mode, prefix) ++ formatName(~mode, name)
    switch mode {
    | MathML => `<mi mathvariant="normal">${body}</mi>`
    | _ => body
    }
  }
)

let toString = (~mode, {prefix, name, power}: t) => {
  let formattedUnit = formatUnit(~mode, ~prefix, ~name)

  switch power {
  | 1 => formattedUnit
  | _ =>
    let power = Belt.Int.toString(power)
    switch mode {
    | MathML => `<msup>${formattedUnit}<mn>${power}</mn></msup>`
    | Tex => `${formattedUnit}^{${power}}`
    | Ascii => `${formattedUnit}^${power}`
    | Unicode => `${formattedUnit}${Formatting_Unicode.formatSuperscriptNumbers(power)}`
    }
  }
}
