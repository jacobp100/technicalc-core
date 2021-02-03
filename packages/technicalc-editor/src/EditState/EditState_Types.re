type t = {
  index: int,
  elements: array(AST.t),
  formatCaptureGroups: bool,
};

let empty = {index: 0, elements: [||], formatCaptureGroups: false};
