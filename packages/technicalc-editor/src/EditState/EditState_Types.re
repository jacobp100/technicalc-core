type t = {
  index: int,
  elements: array(AST.t),
  allowLabelEditing: bool,
};

let empty = {index: 0, elements: [||], allowLabelEditing: false};
