open EditState_Types;
open EditState_Util;

let make = (~index, ~elements, ~allowLabelEditing) => {
  index: preferredInsertionIndex(~index, ~elements, ~allowLabelEditing),
  elements,
  allowLabelEditing,
};

let clear = ({allowLabelEditing}) => {
  index: 0,
  elements: [||],
  allowLabelEditing,
};
