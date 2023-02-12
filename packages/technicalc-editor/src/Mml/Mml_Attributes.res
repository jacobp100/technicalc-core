type mmlNames = [
  | #xmlns
  | #display
  | #mathvariant
  | #stretchy
  | #align
  | #width
]

type customNames = [#id | #class]

type name = [mmlNames | customNames]
type value = string
type t = (name, value)

let toString = (~metadata=true, (name, value): t) => {
  let skip =
    metadata == false &&
      switch name {
      | #...customNames => true
      | _ => false
      }

  if skip {
    None
  } else {
    Some(`${(name :> string)}="${value}"`)
  }
}
