open Formatting_Types

let toString = (~format, {value, units}: Measure.t) => {
  let {mode} = format
  let valueSeparator = switch mode {
  | MathML => "<mspace width=\"0.2em\" />"
  | Tex => `\\;`
  | Ascii | Unicode => " "
  }
  let unitsSeparator = switch mode {
  | MathML => "<mspace width=\"0.1em\" />"
  | Tex => `\\,`
  | Ascii | Unicode => " "
  }

  let valueString = Formatting_Real.toString(~format, value)
  let unitsString = Belt.Array.mapU(units, (. unit) => {
    Formatting_Units.toString(~mode, unit)
  })->StringUtil.joinWith(unitsSeparator)

  valueString ++ valueSeparator ++ unitsString
}
