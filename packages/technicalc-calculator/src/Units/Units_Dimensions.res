open Units_Types

let dimensions = x =>
  Belt.Array.reduceU(x.units, Unit.emptyDimensions, (. accum, unit) => {
    Unit.dimensionsAdd(accum, Unit.dimensions(unit))
  })
