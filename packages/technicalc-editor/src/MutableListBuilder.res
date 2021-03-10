external mutableCell: ('a, list<'a>) => list<'a> = "#makemutablelist"
@set external unsafeMutateTail: (list<'a>, list<'a>) => unit = "tl"

type t<'a> =
  | Empty
  | Initialized({start: list<'a>, mutable end: list<'a>})

let empty = Empty

let append = (list, element) => {
  let next = mutableCell(element, list{})
  switch list {
  | Empty => Initialized({start: next, end: next})
  | Initialized(contents) =>
    unsafeMutateTail(contents.end, next)
    contents.end = next
    list
  }
}

let toList = list =>
  switch list {
  | Empty => list{}
  | Initialized({start}) => start
  }
