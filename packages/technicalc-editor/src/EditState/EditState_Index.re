open EditState_Types;
open EditState_Util;

let setIndex = ({elements, formatCaptureGroups}, index) => {
  let index =
    preferredInsertionIndex(~index, ~elements, ~formatCaptureGroups);
  {index, elements, formatCaptureGroups};
};

let%private moveIndexInDirection =
            (~forwards, {index, elements, formatCaptureGroups}) => {
  let step = forwards ? 1 : (-1);

  let rec iter = index =>
    switch (
      EditState_Util.preferredShiftDirection(
        ~index,
        ~elements,
        ~formatCaptureGroups,
      )
    ) {
    | NoShift =>
      let index =
        preferredInsertionIndex(~index, ~elements, ~formatCaptureGroups);
      {index, elements, formatCaptureGroups};
    | Forwards
    | Backwards => iter(index + step)
    };

  iter(index + step);
};

let previous = s => moveIndexInDirection(~forwards=false, s);

let next = s => moveIndexInDirection(~forwards=true, s);

let moveStart = ({elements, formatCaptureGroups}) => {
  let index =
    preferredInsertionIndex(~index=0, ~elements, ~formatCaptureGroups);
  {index, elements, formatCaptureGroups};
};

let moveEnd = ({elements, formatCaptureGroups}) => {
  let index =
    preferredInsertionIndex(
      ~index=Belt.Array.length(elements),
      ~elements,
      ~formatCaptureGroups,
    );
  {index, elements, formatCaptureGroups};
};
