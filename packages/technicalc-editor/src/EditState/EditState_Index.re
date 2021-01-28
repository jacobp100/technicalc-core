open EditState_Types;
open EditState_Util;

let setIndex = ({elements, allowLabelEditing}, index) => {
  let index = preferredInsertionIndex(~index, ~elements, ~allowLabelEditing);
  {index, elements, allowLabelEditing};
};

let%private moveIndexInDirection =
            (~forwards, {index, elements, allowLabelEditing}) => {
  let step = forwards ? 1 : (-1);

  let rec iter = index =>
    switch (
      EditState_Util.preferredShiftDirection(
        ~index,
        ~elements,
        ~allowLabelEditing,
      )
    ) {
    | NoShift =>
      let index =
        preferredInsertionIndex(~index, ~elements, ~allowLabelEditing);
      {index, elements, allowLabelEditing};
    | Forwards
    | Backwards => iter(index + step)
    };

  iter(index + step);
};

let previous = s => moveIndexInDirection(~forwards=false, s);

let next = s => moveIndexInDirection(~forwards=true, s);

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
