open Units_Types

let eq = (a, b) => a.prefix == b.prefix && a.name == b.name && a.power == b.power

let flatten = (a: array<t>) =>
  Belt.Array.reduceU(a, [], (. accum, current) => {
    let existing = Belt.Array.getIndexByU(accum, (. other) => {
      other.prefix == current.prefix && other.name == current.name
    })

    switch existing {
    | Some(index) =>
      let existing = Belt.Array.getExn(accum, index)
      Belt.Array.setExn(accum, index, {...current, power: current.power + existing.power})
      accum
    | None => Belt.Array.concat(accum, [current])
    }
  })
