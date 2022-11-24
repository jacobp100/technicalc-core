open AST
open Value_Builders

type accum = list<TechniCalcEditor.Value_Types.partialNode>

let parse = (elements: array<t>) => {
  let error = ref(None)

  let reduce = (. elementsRev: accum, element, (i, i')) =>
    if error.contents == None {
      switch element {
      | Fold_Placeholder(_)
      | Fold_CaptureGroupPlaceholder(_) =>
        error := Some(i)
        list{}
      | _ => list{Value_Element.map(element, i, i'), ...elementsRev}
      }
    } else {
      list{}
    }

  let map = (. elementsRev: accum, (i, _)): TechniCalcCalculator.AST_Types.t =>
    if error.contents == None {
      let elements = Belt.List.toArray(elementsRev)
      Belt.Array.reverseInPlace(elements)

      switch Value_Row.parse(elements) {
      | Ok(root) => root
      | Error(index) =>
        error := Some(Belt.Option.getWithDefault(index, i))
        Node.NaN
      }
    } else {
      NaN
    }

  let root = reduceMapU(elements, ~reduce, ~map, ~initial=list{})

  switch error.contents {
  | Some(i) => Error(i)
  | None => Ok(root)
  }
}
