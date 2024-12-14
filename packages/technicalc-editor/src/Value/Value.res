open AST
open Value_Builders
open Value_Types

type accum = list<(AST.foldState<node>, (int, int))>

let parse = (elements: array<t>) => {
  let error = ref(None)

  let reduce = (elementsRev: accum, element, (r, _) as range) =>
    if error.contents == None {
      switch element {
      | Fold_Placeholder(_) =>
        error := Some(r)
        list{}
      | _ => list{(element, range), ...elementsRev}
      }
    } else {
      list{}
    }

  let map = (elementsRev: accum, _): TechniCalcCalculator.AST_Types.t =>
    if error.contents == None {
      let elements = Belt.List.toArray(elementsRev)
      Belt.Array.reverseInPlace(elements)

      switch Value_Map.parse(elements) {
      | Ok(root) => root
      | Error(index) =>
        error := Some(index)
        Node.NaN
      }
    } else {
      NaN
    }

  let root = reduceMap(elements, ~reduce, ~map, ~initial=list{})

  switch error.contents {
  | Some(i) => Error(i)
  | None => Ok(root)
  }
}
