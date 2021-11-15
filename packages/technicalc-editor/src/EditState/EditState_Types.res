type t = {
  index: int,
  elements: array<AST.t>,
  formatCaptureGroups: bool,
}

let empty = {index: 0, elements: [], formatCaptureGroups: false}

let isEmpty = x => Belt.Array.length(x.elements) == 0
