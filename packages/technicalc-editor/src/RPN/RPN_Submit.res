open RPN_Types

let submit = (
  ~pushLashWhenEmpty=true,
  {stackRev} as rpn: t,
  {elements} as editState: EditState.t,
): result<t, int> =>
  if !EditState.isEmpty(editState) {
    switch Value.parse(elements) {
    | Ok(_) => Ok({stackRev: list{elements, ...stackRev}})
    | Error(i) => Error(i)
    }
  } else if pushLashWhenEmpty {
    switch stackRev {
    | list{elements, ...stackRev} => Ok({stackRev: list{elements, elements, ...stackRev}})
    | list{} => Ok(rpn)
    }
  } else {
    Ok(rpn)
  }
