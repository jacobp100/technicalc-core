open EditState_Types;
open EditState_Util;

let setIndex = ({elements, formatCaptureGroups}, index) => {
  let index =
    preferredInsertionIndex(~index, ~elements, ~formatCaptureGroups);
  {index, elements, formatCaptureGroups};
};

let%private moveIndexInDirection =
            (~forwards, {index, elements, formatCaptureGroups}) => {
  /* Note returning an index of length _is_ valid */
  let length = Belt.Array.length(elements);
  let step = forwards ? 1 : (-1);

  let rec iter = index => {
    let preferredShiftDirection =
      EditState_Util.preferredShiftDirection(
        ~index,
        ~elements,
        ~formatCaptureGroups,
      );
    let nextIndex =
      switch (preferredShiftDirection) {
      | Some(_) => Some(index + step)
      | None => None
      };
    switch (nextIndex) {
    | Some(nextIndex) when nextIndex >= 0 && nextIndex <= length =>
      iter(nextIndex)
    | _ =>
      let index =
        preferredInsertionIndex(~index, ~elements, ~formatCaptureGroups);
      {index, elements, formatCaptureGroups};
    };
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
