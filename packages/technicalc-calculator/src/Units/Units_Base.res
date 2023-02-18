open Units_Types

let empty = {units: []}

let isEmpty = x => Belt.Array.length(x.units) == 0

let toArray = x => x.units

%%private(let hasNonZeroPower = (. x: Unit.t) => x.power != 0)

let ofArray = (x: array<Unit.t>): t => {
  let zeroUnits = ref(0)

  let units = Belt.Array.reduceU(x, [], (. accum, current) => {
    let existing = Belt.Array.getIndexByU(accum, (. other: Unit.t) => {
      other.prefix == current.prefix && other.name == current.name
    })

    switch existing {
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

  {units: units}
}
