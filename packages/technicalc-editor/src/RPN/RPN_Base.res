open RPN_Types

let empty = {stackRev: list{}}

let elements = x => Belt.List.head(x.stackRev)

let stack = x => Belt.List.toArray(x.stackRev)

let depth = x => Belt.List.length(x.stackRev)
