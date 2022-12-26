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
