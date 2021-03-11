type t<'a> =
  | Empty
  | Initialized({mutable listRev: list<'a>, mutable length: int})

let empty = Empty

let append = (x, element) =>
  switch x {
  | Empty => Initialized({listRev: list{element}, length: 0})
  | Initialized(contents) =>
    contents.listRev = list{element, ...contents.listRev}
    contents.length = contents.length + 1
    x
  }

let length = x =>
  switch x {
  | Empty => 0
  | Initialized({length}) => length
  }

let toArray = x =>
  switch x {
  | Empty => []
  | Initialized({listRev, length}) =>
    let rec iter = (output, current, index) =>
      switch current {
      | list{element, ...tail} =>
        Belt.Array.setUnsafe(output, index, element)
        iter(output, tail, index - 1)
      | list{} => output
      }

    let output = Belt.Array.makeUninitializedUnsafe(length)
    iter(output, listRev, length)
  }
