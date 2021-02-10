let%private addDependency = (dependency: string, dependencies) => {
  let contained = Belt.List.someU(dependencies, (. x) => x == dependency);
  contained ? dependencies : [dependency, ...dependencies];
};

let getDependencies = (elements: array(AST.t)) => {
  let rec iter = (~dependencies, i) =>
    switch (Belt.Array.get(elements, i)) {
    | Some(VariableS({id})) =>
      let dependencies = addDependency(id, dependencies);
      iter(~dependencies, i + 1);
    | Some(_) => iter(~dependencies, i + 1)
    | None => Belt.List.toArray(dependencies)
    };

  iter(~dependencies=[], 0);
};
