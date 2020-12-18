type bracketGroup = {
  openBracketIndex: int,
  func: option(Value_Types.funcitionLike),
  elements: MutableListBuilder.t(Value_Types.partialNode),
};

type t = {
  level0Body: MutableListBuilder.t(Value_Types.partialNode),
  bracketGroups: list(bracketGroup),
};

let empty = {level0Body: MutableListBuilder.empty, bracketGroups: []};

let append = (v, element) =>
  switch (v.bracketGroups) {
  | [bracketGroup, ...rest] =>
    let nextBracketGroupElements =
      MutableListBuilder.append(bracketGroup.elements, element);

    if (bracketGroup.elements === nextBracketGroupElements) {
      v;
    } else {
      let nextBracketGroup = {
        ...bracketGroup,
        elements: nextBracketGroupElements,
      };
      {...v, bracketGroups: [nextBracketGroup, ...rest]};
    };
  | [] =>
    let nextLevel0Body = MutableListBuilder.append(v.level0Body, element);

    if (v.level0Body === nextLevel0Body) {
      v;
    } else {
      {level0Body: nextLevel0Body, bracketGroups: []};
    };
  };

let appendOpenBracket = ({level0Body, bracketGroups}, openBracketIndex, func) => {
  level0Body,
  bracketGroups: [
    {openBracketIndex, func, elements: MutableListBuilder.empty},
    ...bracketGroups,
  ],
};

let appendCloseBracket = ({level0Body, bracketGroups}) =>
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
  | [{openBracketIndex}, ..._] => Error(openBracketIndex)
  };
