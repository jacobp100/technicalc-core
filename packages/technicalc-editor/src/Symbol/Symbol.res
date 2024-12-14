type t = {
  bold: bool,
  italic: bool,
  base: string,
  subscript: string,
  superscript: string,
}

let eq = (a, b) =>
  a.bold == b.bold &&
  a.italic == b.italic &&
  a.base == b.base &&
  a.superscript == b.superscript &&
  a.subscript == b.subscript

let empty = {
  bold: false,
  italic: true,
  base: "",
  subscript: "",
  superscript: "",
}

let ofString = str => {
  bold: false,
  italic: String.length(str) <= 1,
  base: str,
  subscript: "",
  superscript: "",
}

let isEmpty = value =>
  String.length(value.base) === 0 &&
  String.length(value.subscript) === 0 &&
  String.length(value.superscript) === 0

let isValid = value => String.length(value.base) !== 0

%%private(
  let aliasCharacter = value =>
    switch Obj.magic(StringUtil.charAtUnsafe(value, 0)) {
    | '0' => "0"
    | '1' => "1"
    | '2' => "2"
    | '3' => "3"
    | '4' => "4"
    | '5' => "5"
    | '6' => "6"
    | '7' => "7"
    | '8' => "8"
    | '9' => "9"
    | '-' => "-"
    | '*' => "star"
    | '′' => "prime"
    | 'A' | 'a' => "a"
    | 'B' | 'b' => "b"
    | 'C' | 'c' => "c"
    | 'D' | 'd' => "d"
    | 'E' | 'e' => "e"
    | 'F' | 'f' => "f"
    | 'G' | 'g' => "g"
    | 'H' | 'h' => "h"
    | 'I' | 'i' => "i"
    | 'J' | 'j' => "j"
    | 'K' | 'k' => "k"
    | 'L' | 'l' => "l"
    | 'M' | 'm' => "m"
    | 'N' | 'n' => "n"
    | 'O' | 'o' => "o"
    | 'P' | 'p' => "p"
    | 'Q' | 'q' => "q"
    | 'R' | 'r' => "r"
    | 'S' | 's' => "s"
    | 'T' | 't' => "t"
    | 'U' | 'u' => "u"
    | 'V' | 'v' => "v"
    | 'W' | 'w' => "w"
    | 'X' | 'x' => "x"
    | 'Y' | 'y' => "y"
    | 'Z' | 'z' => "z"
    | 'Α' | 'α' => "alpha"
    | 'Β' | 'β' => "beta"
    | 'Γ' | 'γ' => "gamma"
    | 'Δ' | 'δ' => "delta"
    | 'Ε' | 'ε' => "epsilon"
    | 'Ζ' | 'ζ' => "zeta"
    | 'Η' | 'η' => "eta"
    | 'Θ' | 'θ' => "theta"
    | 'Ι' | 'ι' => "iota"
    | 'Κ' | 'κ' => "kappa"
    | 'Λ' | 'λ' => "lambda"
    | 'Μ' | 'μ' => "mu"
    | 'Ν' | 'ν' => "nu"
    | 'Ξ' | 'ξ' => "xi"
    | 'Ο' | 'ο' => "omicron"
    | 'Π' | 'π' => "pi"
    | 'Ρ' | 'ρ' => "rho"
    | 'Σ'
    | 'ς'
    | 'σ' => "sigma"
    | 'Τ' | 'τ' => "tau"
    | 'Υ' | 'υ' => "upsilon"
    | 'Φ' | 'φ' => "phi"
    | 'Χ' | 'χ' => "chi"
    | 'Ψ' | 'ψ' => "psi"
    | 'Ω' | 'ω' => "omega"
    | 'ħ' | 'ℏ' => "hbar"
    | '∞' => "infinity"
    | _ => ""
    }
)

%%private(
  let aliasPart = value =>
    StringUtil.split(value, ~separator="")->Belt.Array.map(aliasCharacter)->StringUtil.join
)

let alias = value =>
  aliasPart(value.base) ++ aliasPart(value.subscript) ++ aliasPart(value.superscript)
