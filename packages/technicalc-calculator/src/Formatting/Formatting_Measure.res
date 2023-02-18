open Formatting_Types

let formatUnits = (~mode, units) => {
  let unitsSeparator = switch mode {
  | MathML => "<mspace width=\"0.1666em\" />"
  | Tex => `\\,`
  | Ascii | Unicode => " "
  }

  Units_Base.toArray(units)
  ->Belt.Array.mapU((. unit) => {
    Formatting_Unit.toString(~mode, unit)
  })
  ->StringUtil.joinWith(unitsSeparator)
}

let toString = (~format, {value, units}: Measure.t) => {
  let {mode} = format
  let valueSeparator = switch mode {
  | MathML => "<mspace width=\"0.2222em\" />"
  | Tex => `\\;`
  | Ascii | Unicode => " "
  }

  Formatting_Real.toString(~format, value) ++ valueSeparator ++ formatUnits(~mode, units)
}
