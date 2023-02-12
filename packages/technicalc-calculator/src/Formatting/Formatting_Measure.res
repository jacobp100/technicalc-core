open Formatting_Types

let toString = (~format, {value, units}: Measure.t) => {
  let valueSeparator = switch format.mode {
  | MathML => "<mspace width=\"0.2em\" />"
  | Tex => `\\;`
  | Ascii | Unicode => " "
  }
  let unitsSeparator = switch format.mode {
  | MathML => "<mspace width=\"0.1em\" />"
  | Tex => `\\,`
  | Ascii | Unicode => " "
  }

  let valueString = Formatting_Real.toString(~format, value)
  let unitsString = Belt.Array.mapU(units, (. unit) => {
    Formatting_Units.toString(~format, unit)
  })->StringUtil.joinWith(unitsSeparator)

  valueString ++ valueSeparator ++ unitsString
}
