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
