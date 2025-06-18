open Units_Types
open Units_Value

%%private(
  let celsiusToKelvin = value => {
    open Real
    value + ofFloat(273.15)
  }
)
%%private(
  let fahrenheitToKelvin = value => {
    open Real
    (value - ofFloat(32.)) / ofFloat(1.8) + ofFloat(273.15)
  }
)
%%private(
  let celsiusFromKelvin = value => {
    open Real
    value - ofFloat(273.15)
  }
)
%%private(
  let fahrenheitFromKelvin = value => {
    open Real
    (value - ofFloat(273.15)) * ofFloat(1.8) + ofFloat(32.)
  }
)

%%private(
  let transformUnits = (
    ~transformCelsius,
    ~transformFahrenheit,
    ~powerMultiplier,
    value: Real.t,
    units: array<t>,
  ) => {
    open Real
    let handleLinearUnit = (value, unitPart) =>
      switch unitPart.name {
      | Celsius
      | Fahrenheit => nan
      | _ => value * conversionFactorExn(~powerMultiplier, unitPart)
      }

    switch units {
    | [{prefix, name: Celsius, power: 1}] => transformCelsius(prefixValue(prefix) * value)
    | [{prefix, name: Fahrenheit, power: 1}] => transformFahrenheit(prefixValue(prefix) * value)
    | _ => Belt.Array.reduce(units, value, handleLinearUnit)
    }
  }
)

%%private(
  let toSi = (value: Real.t, units) =>
    transformUnits(
      ~transformCelsius=celsiusToKelvin,
      ~transformFahrenheit=fahrenheitToKelvin,
      ~powerMultiplier=1,
      value,
      units,
    )
)

%%private(
  let fromSi = (value: Real.t, units) =>
    transformUnits(
      ~transformCelsius=celsiusFromKelvin,
      ~transformFahrenheit=fahrenheitFromKelvin,
      ~powerMultiplier=-1,
      value,
      units,
    )
)

let convert = (value: Real.t, ~fromUnits, ~toUnits) =>
  if Units_Compatibility.compatible(~fromUnits, ~toUnits) {
    value->toSi(fromUnits)->fromSi(toUnits)
  } else {
    Real.nan
  }

%%private(let compositeCmp = (a, b) => Real.cmp(conversionFactorExn(b), conversionFactorExn(a)))
%%private(
  let compositeSorted = (a: array<t>) => Belt.Array.copy(a)->ArrayUtil.sortInPlace(compositeCmp)
)

let convertComposite = (values: array<(Real.t, t)>, ~toUnits: array<t>) => {
  let fromUnits = Belt.Array.map(values, ((_, unitPart)) => unitPart)
  let valueSi = Belt.Array.reduce(values, Real.zero, (accum, (value, unitPart)) => {
    open Real
    accum + value * conversionFactorExn(~powerMultiplier=1, unitPart)
  })

  if !Units_Compatibility.compositeCompatible(~fromUnits, ~toUnits) {
    None
  } else if Belt.Array.length(toUnits) == 1 {
    // Don't truncate value
    let unitPart = Belt.Array.getExn(toUnits, 0)
    let value = Real.mul(valueSi, conversionFactorExn(~powerMultiplier=-1, unitPart))
    Some([(value, unitPart)])
  } else {
    let toUnits = compositeSorted(toUnits)
    let remainderSi = ref(valueSi)
    let output = Belt.Array.map(toUnits, unitPart => {
      let value = Real.mul(remainderSi.contents, conversionFactorExn(~powerMultiplier=-1, unitPart))

      let valueFloor = Real.trunc(value)
      let remainder = Real.sub(value, valueFloor)
      remainderSi := Real.mul(remainder, conversionFactorExn(~powerMultiplier=1, unitPart))
      let value = valueFloor

      (value, unitPart)
    })
    Some(output)
  }
}
