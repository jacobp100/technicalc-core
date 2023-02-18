open Units_Types

%%private(
  let celsiusToKelvinU = (. value) => {
    open Real
    value + ofFloat(273.15)
  }
)
%%private(
  let fahrenheitToKelvinU = (. value) => {
    open Real
    (value - ofFloat(32.)) / ofFloat(1.8) + ofFloat(273.15)
  }
)
%%private(
  let celsiusFromKelvinU = (. value) => {
    open Real
    value - ofFloat(273.15)
  }
)
%%private(
  let fahrenheitFromKelvinU = (. value) => {
    open Real
    (value - ofFloat(273.15)) * ofFloat(1.8) + ofFloat(32.)
  }
)

%%private(
  let transformUnits = (
    ~transformCelsiusU,
    ~transformFahrenheitU,
    ~powerMultiplier,
    value: Real.t,
    {units}: t,
  ) => {
    open Real
    let handleLinearUnitU = (. value, unitPart: Unit.t) =>
      switch unitPart.name {
      | Celsius
      | Fahrenheit => nan
      | _ => value * Unit.conversionFactorExn(~powerMultiplier, unitPart)->Real.ofDecimal
      }

    switch units {
    | [{prefix, name: Celsius, power: 1}] =>
      transformCelsiusU(. Unit.prefixValue(prefix)->Real.ofDecimal * value)
    | [{prefix, name: Fahrenheit, power: 1}] =>
      transformFahrenheitU(. Unit.prefixValue(prefix)->Real.ofDecimal * value)
    | _ => Belt.Array.reduceU(units, value, handleLinearUnitU)
    }
  }
)

%%private(
  let toSi = (value: Real.t, x) =>
    transformUnits(
      ~transformCelsiusU=celsiusToKelvinU,
      ~transformFahrenheitU=fahrenheitToKelvinU,
      ~powerMultiplier=1,
      value,
      x,
    )
)

%%private(
  let fromSi = (value: Real.t, x) =>
    transformUnits(
      ~transformCelsiusU=celsiusFromKelvinU,
      ~transformFahrenheitU=fahrenheitFromKelvinU,
      ~powerMultiplier=-1,
      value,
      x,
    )
)

let convert = (value: Real.t, ~fromUnits, ~toUnits) =>
  if Units_Compatibility.compatible(~fromUnits, ~toUnits) {
    value->toSi(fromUnits)->fromSi(toUnits)
  } else {
    Real.nan
  }

%%private(
  let compositeCmp = (a, b) => Decimal.cmp(Unit.conversionFactorExn(b), Unit.conversionFactorExn(a))
)
%%private(
  let compositeSorted = (x: t) => Belt.Array.copy(x.units)->ArrayUtil.sortInPlace(compositeCmp)
)

let convertComposite = (values: array<(Real.t, Unit.t)>, ~toUnits: t) => {
  let fromUnits = {units: Belt.Array.mapU(values, (. (_, unitPart)) => unitPart)}

  if Units_Compatibility.compositeCompatible(~fromUnits, ~toUnits) {
    let valueSi = Belt.Array.reduceU(values, Real.zero, (. accum, (value, unitPart)) => {
      open Real
      accum + value * Unit.conversionFactorExn(~powerMultiplier=1, unitPart)->Real.ofDecimal
    })
    let toUnits = compositeSorted(toUnits)
    let remainderSi = ref(valueSi)
    let output = Belt.Array.mapU(toUnits, (. unitPart) => {
      let value = Real.mul(
        remainderSi.contents,
        Unit.conversionFactorExn(~powerMultiplier=-1, unitPart)->Real.ofDecimal,
      )

      let valueFloor = Real.trunc(value)
      let remainder = Real.sub(value, valueFloor)
      remainderSi :=
        Real.mul(remainder, Unit.conversionFactorExn(~powerMultiplier=1, unitPart)->Real.ofDecimal)
      let value = valueFloor

      (value, unitPart)
    })
    Some(output)
  } else {
    None
  }
}
