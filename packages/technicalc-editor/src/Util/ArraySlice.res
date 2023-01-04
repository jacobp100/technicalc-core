type slice<'a> = {
  @as("a") array: array<'a>,
  @as("o") offset: int,
  @as("l") len: int,
}

type t<'a> = option<slice<'a>>

let ofArray = (array: array<'a>): t<'a> => Some({
  array,
  offset: 0,
  len: Belt.Array.length(array),
})

let toArray = (x: t<'a>): array<'a> =>
  switch x {
  | Some({array, len, offset}) => Belt.Array.slice(array, ~len, ~offset)
  | None => []
  }

let length = (x: t<'a>) =>
  switch x {
  | Some(x) => x.len
  | None => 0
  }

let get = (x: t<'a>, index: int): option<'a> =>
  switch x {
  | Some(x) if index >= 0 && index < x.len => Some(Belt.Array.getUnsafe(x.array, x.offset + index))
  | _ => None
  }

let slice = (x: t<'a>, ~offset: int, ~len: int): t<'a> =>
  switch x {
  | Some(x) =>
    let start = x.offset + (offset >= 0 ? offset : x.len + offset)
    let proposedEnd = start + len
    let maximumEnd = x.offset + x.len
    let end = min(proposedEnd, maximumEnd)
    if start < end {
      Some({array: x.array, offset: start, len: end - start})
    } else {
      None
    }
  | None => None
  }

let sliceToEnd = (x: t<'a>, offset: int): t<'a> =>
  if offset == 0 {
    x
  } else {
    switch x {
    | Some(x) =>
      let start = x.offset + (offset >= 0 ? offset : x.len + offset)
      let end = x.offset + x.len
      if start < end {
        Some({array: x.array, offset: start, len: end - start})
      } else {
        None
      }
    | None => None
    }
  }
