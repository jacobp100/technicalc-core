open EditState_Types;
open EditState_Util;

let make = (~index, ~elements, ~formatCaptureGroups) => {
  index: preferredInsertionIndex(~index, ~elements, ~formatCaptureGroups),
  elements,
  formatCaptureGroups,
};

let clear = ({formatCaptureGroups}) => {
  index: 0,
  elements: [||],
  formatCaptureGroups,
};
