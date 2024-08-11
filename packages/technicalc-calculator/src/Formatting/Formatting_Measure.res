open Formatting_Types

let formatUnits = (~mode, ~unitFormat=defaultUnitFormat, units) => {
  let unitsSeparator = switch unitFormat {
  | Exponential =>
    switch mode {
    | MathML => "<mspace width=\"0.1666em\" />"
    | Tex => `\\,`
    | Ascii | Unicode => " "
    }
  | Operator => ""
  }

  Belt.Array.mapU(units, (. unit) => {
    Formatting_Units.toString(~mode, ~unitFormat, unit)
  })->StringUtil.joinWith(unitsSeparator)
}

let toString = (~format, ~unitFormat=?, {value, units}: Measure.t) => {
  let {mode} = format
  let valueSeparator = switch mode {
  | MathML => "<mspace width=\"0.2222em\" />"
  | Tex => `\\;`
  | Ascii | Unicode => " "
  }

  Formatting_Real.toString(~format, value) ++
  valueSeparator ++
  formatUnits(~mode, ~unitFormat?, units)
}
