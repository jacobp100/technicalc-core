open RPN_Types

let drop = (x: t): option<t> =>
  switch x.stackRev {
  | list{_, ...stackRev} => Some({stackRev: stackRev})
  | list{} => None
  }

let swap = (x: t): option<t> =>
  switch x.stackRev {
  | list{a, b, ...stackRev} => Some({stackRev: list{b, a, ...stackRev}})
  | _ => None
  }
