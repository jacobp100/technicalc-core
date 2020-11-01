type bracketGroup = {
  index: int,
  func: option(Value_Types.funcitionLike),
  elements: MutableListBuilder.t(Value_Types.partialNode),
};

type t = {
  level0Body: MutableListBuilder.t(Value_Types.partialNode),
  bracketGroups: list(bracketGroup),
};

let empty = {level0Body: MutableListBuilder.empty, bracketGroups: []};

let append = (accum, element) =>
  switch (accum) {
  | {level0Body, bracketGroups: []} =>
    let nextLevel0Body = MutableListBuilder.append(level0Body, element);
    if (level0Body === nextLevel0Body) {
      accum;
    } else {
      {level0Body: nextLevel0Body, bracketGroups: []};
    };
  | {bracketGroups: [bracketGroup, ...rest]} =>
    let bracketGroup = {
      ...bracketGroup,
      elements: MutableListBuilder.append(bracketGroup.elements, element),
    };
    {...accum, bracketGroups: [bracketGroup, ...rest]};
  };

let openBracket = ({level0Body, bracketGroups}, index, func) => {
  level0Body,
  bracketGroups: [
    {index, func, elements: MutableListBuilder.empty},
    ...bracketGroups,
  ],
};

let closeBracket = ({level0Body, bracketGroups}) =>
  switch (bracketGroups) {
  | [{func, elements}, ...rest] =>
    Some((
      {level0Body, bracketGroups: rest},
      func,
      MutableListBuilder.toList(elements),
    ))
  | _ => None
  };

let toList = ({level0Body, bracketGroups}) =>
  switch (bracketGroups) {
  | [] => Ok(MutableListBuilder.toList(level0Body))
  | [{index}, ..._] => Error(index)
  };
