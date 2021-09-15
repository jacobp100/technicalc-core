open TechniCalcCalculator.Unit_Types

let prefixes = [
  Unit,
  Femto,
  Pico,
  Nano,
  Micro,
  Milli,
  Centi,
  Deci,
  Deca,
  Hecto,
  Kilo,
  Mega,
  Giga,
  Tera,
  Peta,
  Exa,
  Kibi,
  Mebi,
  Gibi,
  Tebi,
  Pebi,
  Exbi,
]

let prefixToString = (prefix: prefix) =>
  switch prefix {
  | Femto => Some("Femto")
  | Pico => Some("Pico")
  | Nano => Some("Nano")
  | Micro => Some("Micro")
  | Milli => Some("Milli")
  | Centi => Some("Centi")
  | Deci => Some("Deci")
  | Unit => None
  | Deca => Some("Deca")
  | Hecto => Some("Hecto")
  | Kilo => Some("Kilo")
  | Mega => Some("Mega")
  | Giga => Some("Giga")
  | Tera => Some("Tera")
  | Peta => Some("Peta")
  | Exa => Some("Exa")
  | Kibi => Some("Kibi")
  | Mebi => Some("Mebi")
  | Gibi => Some("Gibi")
  | Tebi => Some("Tebi")
  | Pebi => Some("Pebi")
  | Exbi => Some("Exbi")
  }

let unitToString = (unit: unitType) =>
  switch unit {
  | Second => "Second"
  | Minute => "Minute"
  | Hour => "Hour"
  | Day => "Day"
  | Week => "Week"
  | Month => "Month"
  | Year => "Year"
  | Decade => "Decade"
  | Century => "Century"
  | Meter => "Meter"
  | Inch => "Inch"
  | Foot => "Foot"
  | Yard => "Yard"
  | Mile => "Mile"
  | NauticalMile => "Nautical Mile"
  | LightYear => "Light Year"
  | Parsec => "Parsec"
  | Angstrom => "Angstrom"
  | Gram => "Gram"
  | Tonne => "Tonne"
  | Ounce => "Ounce"
  | Pound => "Pound"
  | Stone => "Stone"
  | Acre => "Acre"
  | Hectare => "Hectare"
  | Liter => "Liter"
  | Gallon => "Gallon"
  | USGallon => "US Gallon"
  | Quart => "Quart"
  | Cup => "Cup"
  | USCup => "US Cup"
  | Teaspoon => "Teaspoon"
  | Tablespoon => "Tablespoon"
  | FluidOunce => "Fluid Ounce"
  | Knot => "Knot"
  | Newton => "Newton"
  | PoundForce => "Pound Force"
  | Pascal => "Pascal"
  | Atmosphere => "Atmosphere"
  | Bar => "Bar"
  | Joule => "Joule"
  | Calorie => "Calorie"
  | ElectronVolt => "Electron Volt"
  | BTU => "BTU"
  | Therm => "Therm"
  | Watt => "Watt"
  | Horsepower => "Horsepower"
  | MetricHorsepower => "Metric Horsepower"
  | Bit => "Bit"
  | Byte => "Byte"
  | Kelvin => "Kelvin"
  | Celsius => "Celsius"
  | Fahrenheit => "Fahrenheit"
  }

let unitToStringPlural = (unit: unitType) =>
  switch unit {
  | Century => "Centuries"
  | Inch => "Inches"
  | Foot => "Feet"
  | Celsius
  | Fahrenheit =>
    unitToString(unit)
  | _ => unitToString(unit) ++ "s"
  }

%%private(
  let unitPartToString = (~plural, {prefix, unit, power}: unitPart) => {
    let out = plural ? unitToStringPlural(unit) : unitToString(unit)

    let out = switch prefixToString(prefix) {
    | Some(prefix) =>
      let secondLetter = Js.String.get(out, 1)
      let firstWordIsUpperCase = secondLetter == Js.String.toUpperCase(secondLetter)

      if firstWordIsUpperCase {
        prefix ++ " " ++ out
      } else {
        let firstLetter = Js.String.get(out, 0)
        prefix ++ Js.String.toLowerCase(firstLetter) ++ Js.String.sliceToEnd(~from=1, out)
      }
    | None => out
    }

    let out = switch abs(power) {
    | 1 => out
    | 2 => "Square " ++ out
    | 3 => "Cubic " ++ out
    | power => out ++ "^" ++ Belt.Int.toString(power)
    }

    out
  }
)

let unitPartsToString = (unitParts: array<unitPart>) =>
  Belt.Array.mapWithIndexU(unitParts, (. index, unitPart) => {
    let plural = switch unitPart.power {
    | 1 =>
      switch Belt.Array.get(unitParts, index + 1) {
      | Some(nextUnitPart) => nextUnitPart.power < 0
      | None => false
      }
    | -1 => false
    | _ => true
    }
    let out = unitPartToString(~plural, unitPart)

    if unitPart.power < 0 {
      "per " ++ out
    } else {
      out
    }
  })->Js.Array.joinWith(" ", _)

let prefixValueToMml = (prefix: prefix) => {
  let maybeBase = switch prefix {
  | Femto => Some("10")
  | Pico => Some("10")
  | Nano => Some("10")
  | Micro => Some("10")
  | Milli => Some("10")
  | Centi => Some("10")
  | Deci => Some("10")
  | Unit => None
  | Deca => Some("10")
  | Hecto => Some("10")
  | Kilo => Some("10")
  | Mega => Some("10")
  | Giga => Some("10")
  | Tera => Some("10")
  | Peta => Some("10")
  | Exa => Some("10")
  | Kibi => Some("2")
  | Mebi => Some("2")
  | Gibi => Some("2")
  | Tebi => Some("2")
  | Pebi => Some("2")
  | Exbi => Some("2")
  }
  let maybeSuperscript = switch prefix {
  | Femto => Some("-15")
  | Pico => Some("-12")
  | Nano => Some("-9")
  | Micro => Some("-6")
  | Milli => Some("-3")
  | Centi => Some("-2")
  | Deci => Some("-1")
  | Unit => None
  | Deca => Some("1")
  | Hecto => Some("2")
  | Kilo => Some("3")
  | Mega => Some("6")
  | Giga => Some("9")
  | Tera => Some("12")
  | Peta => Some("15")
  | Exa => Some("15")
  | Kibi => Some("10")
  | Mebi => Some("20")
  | Gibi => Some("30")
  | Tebi => Some("40")
  | Pebi => Some("50")
  | Exbi => Some("60")
  }

  switch (maybeBase, maybeSuperscript) {
  | (Some(base), Some(superscript)) => Some(`<msup><mn>${base}</mn><mn>${superscript}</mn></msup>`)
  | _ => None
  }
}
