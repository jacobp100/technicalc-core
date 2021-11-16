open RPN_Types

let empty = {stackRev: list{}}

let elements = x => Belt.List.head(x.stackRev)

let depth = x => {
  let length = Belt.List.length(x.stackRev)
  length > 0 ? Some(length - 1) : None
}

let stack = x => Belt.List.toArray(x.stackRev)
