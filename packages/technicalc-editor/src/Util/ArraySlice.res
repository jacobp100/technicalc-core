type t<'a> = {
  array: array<'a>,
  offset: int,
  len: int,
}

let ofArray = array => Some({
  array: array,
  offset: 0,
  len: Belt.Array.length(array),
})

let toArray = x =>
  switch x {
  | Some({array, len, offset}) => Belt.Array.slice(array, ~len, ~offset)
  | None => []
  }

let get = (x, index) =>
  switch x {
  | Some(x) if index >= 0 && index < x.len => Some(Belt.Array.getUnsafe(x.array, x.offset + index))
  | _ => None
  }

let slice = (x, ~offset, ~len) =>
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

let sliceToEnd = (x, offset) =>
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
