open Units_Types

let eq = (a, b) => a.prefix == b.prefix && a.name == b.name && a.power == b.power

%%private(let hasNonZeroPower = (. x: t) => x.power != 0)

let flatten = (x: array<t>) => {
  let zeroUnits = ref(0)

  let units = Belt.Array.reduceU(x, [], (. accum, current) => {
    let existingIndex = Belt.Array.getIndexByU(accum, (. other) => {
      other.prefix == current.prefix && other.name == current.name
    })

    switch existingIndex {
    | Some(index) =>
      let existing = Belt.Array.getExn(accum, index)
      let nextPower = current.power + existing.power
      if existing.power == 0 && nextPower != 0 {
        zeroUnits := zeroUnits.contents - 1
      } else if existing.power != 0 && nextPower == 0 {
        zeroUnits := zeroUnits.contents + 1
      }
      Belt.Array.setExn(accum, index, {...current, power: nextPower})
      accum
    | None => Belt.Array.concat(accum, [current])
    }
  })

  let units = zeroUnits.contents == 0 ? units : Belt.Array.keepU(units, hasNonZeroPower)

  units
}
