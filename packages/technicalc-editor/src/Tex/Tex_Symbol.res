let toTex = (x: Symbol.t) => {
  let tex = x.base
  let tex = x.bold ? `\\textbf${tex}` : tex
  let tex = x.italic ? `\\textit${tex}` : tex
  let tex = x.subscript != "" ? `${tex}_${x.subscript}` : tex
  let tex = x.superscript != "" ? `${tex}^${x.superscript}` : tex
  tex
}
