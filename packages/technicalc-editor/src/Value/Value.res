open AST
open Value_Builders

let parse = (elements: array<t>) => {
  let error = ref(None)

  let reduce = (accum, element, (i, i')) =>
    if error.contents == None {
      switch element {
      | Placeholder(_)
      | CaptureGroupPlaceholder(_) =>
        error := Some(i)
        MutableListBuilder.empty
      | _ =>
        let value = Value_Element.map(element, i, i')
        MutableListBuilder.append(accum, value)
      }
    } else {
      MutableListBuilder.empty
    }

  let map = (accum, (i, _)): TechniCalcCalculator.AST_Types.t =>
    if error.contents == None {
      let elements = MutableListBuilder.toList(accum)
      switch Value_Row.next(elements) {
      | Ok(root) => root
      | Error(i) =>
        error := Some(i)
        Node.NaN
      | UnknownError =>
        error := Some(i)
        NaN
      }
    } else {
      NaN
    }

  let root = reduceMap(elements, ~reduce, ~map, ~initial=MutableListBuilder.empty)

  switch error.contents {
  | Some(i) => Error(i)
  | None => Ok(root)
  }
}
