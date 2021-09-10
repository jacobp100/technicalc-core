@scope("Math") @val external abs: float => float = "abs"

// Checks int boundaries too
let isInt = f => f->Belt.Int.fromFloat->Belt.Float.fromInt == f

// Converts to an int with no rounding, or returns None.
// E.g. 2 -> Some(2), 2.5 -> None
let asInt = f => f->Belt.Int.fromFloat->Belt.Float.fromInt == f ? Some(f->Belt.Int.fromFloat) : None

let asIntExn = f => {
  assert (f->Belt.Int.fromFloat->Belt.Float.fromInt == f)
  Belt.Int.fromFloat(f)
}

// Only used in one place, don't bother inlining stuff for perf here
let toInt = f => floor(f)->asInt

@scope("Number") @val external isFinite: float => bool = "isFinite"

@scope("Math") @val external random: unit => float = "random"
