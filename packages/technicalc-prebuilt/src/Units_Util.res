open TechniCalcCalculator.Units_Types

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
  switch Belt.Array.getExn(Units_Util_Eval.prefixes, Obj.magic(prefix)) {
  | "" => None
  | prefix => Some(prefix)
  }

let nameToString = (name: name) => Belt.Array.getExn(Units_Util_Eval.names, Obj.magic(name))

let nameToStringPlural = (name: name) =>
  switch name {
  | Century => "Centuries"
  | Inch => "Inches"
  | Foot => "Feet"
  | Henry => "Henries"
  | Siemens
  | Hertz
  | Celsius
  | Fahrenheit
  | Kelvin =>
    nameToString(name)
  | _ => nameToString(name) ++ "s"
  }

%%private(
  let unitToString = (~plural, {prefix, name, power}: t) => {
    let out = plural ? nameToStringPlural(name) : nameToString(name)

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

    let out = switch TechniCalcCalculator.IntUtil.abs(power) {
    | 1 => out
    | 2 => "Square " ++ out
    | 3 => "Cubic " ++ out
    | power => out ++ "^" ++ Belt.Int.toString(power)
    }

    out
  }
)

let toString = (units: array<t>) =>
  Belt.Array.mapWithIndexU(units, (. index, unit) => {
    let plural = switch unit.power {
    | 1 =>
      switch Belt.Array.get(units, index + 1) {
      | Some(nextUnitPart) => nextUnitPart.power < 0
      | None => false
      }
    | -1 => false
    | _ => true
    }
    let out = unitToString(~plural, unit)

    if unit.power < 0 {
      "per " ++ out
    } else {
      out
    }
  })->Js.Array.joinWith(" ", _)

let prefixToMml = (prefix: prefix) => {
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
