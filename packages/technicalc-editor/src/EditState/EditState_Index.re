open EditState_Types;
open EditState_Util;

let setIndex = ({elements, allowLabelEditing}, index) => {
  let index = preferredInsertionIndex(~index, ~elements, ~allowLabelEditing);
  {index, elements, allowLabelEditing};
};

let previous = ({index, elements, allowLabelEditing}) => {
  let index =
    preferredInsertionIndex(~index=index - 1, ~elements, ~allowLabelEditing);
  {index, elements, allowLabelEditing};
};

let next = ({index, elements, allowLabelEditing}) => {
  let index =
    if (allowLabelEditing) {
      index + 1;
    } else {
      // This can be less strict than in preferredInsertionIndex
      // Because those checks will be done below
      let rec iter = index =>
        if (!EditState_Util.indexIsValid(elements, index)) {
          iter(index + 1);
        } else {
          index;
        };

      iter(index + 1);
    };

  // Move backwards if we're still in an invalid position
  let index = preferredInsertionIndex(~index, ~elements, ~allowLabelEditing);
  {index, elements, allowLabelEditing};
};

let moveStart = ({elements, allowLabelEditing}) => {
  let index =
    preferredInsertionIndex(~index=0, ~elements, ~allowLabelEditing);
  {index, elements, allowLabelEditing};
};

let moveEnd = ({elements, allowLabelEditing}) => {
  let index =
    preferredInsertionIndex(
      ~index=Belt.Array.length(elements),
      ~elements,
      ~allowLabelEditing,
    );
  {index, elements, allowLabelEditing};
};
