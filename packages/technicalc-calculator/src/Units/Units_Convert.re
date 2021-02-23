open Unit_Types;

let%private prefixValue = (prefix: prefix) =>
  switch (prefix) {
  | Femto => 1e-15
  | Pico => 1e-12
  | Nano => 1e-9
  | Micro => 1e-6
  | Milli => 1e-3
  | Centi => 1e-2
  | Deci => 1e-1
  | Unit => 1e0
  | Deca => 1e1
  | Hecto => 1e2
  | Kilo => 1e3
  | Mega => 1e6
  | Giga => 1e9
  | Tera => 1e12
  | Peta => 1e15
  | Kibi => 1024.
  | Mebi => 1048576.
  | Gibi => 1073741824.
  | Tebi => 1099511627776.
  | Pebi => 1125899906842624.
  };

let%private unitLinearValueExn = (unit: unitType) =>
  switch (unit) {
  /* Time */
  | Second => 1.
  | Minute => 60.
  | Hour => 3600.
  | Day => 86400.
  | Week => 604800.
  | Month => 2628000.
  | Year => 31536000.
  | Decade => 315360000.
  | Century => 3155673600.
  /* Length */
  | Meter => 1.
  | Inch => 0.0254
  | Foot => 0.3048
  | Yard => 0.9144
  | Mile => 1609.
  | NauticalMile => 1852.
  | LightYear => 9.4605284e15
  | Parsec => 3.086e16
  | Angstrom => 1e-10
  /* Mass */
  | Gram => 1e-3
  | Tonne => 1e3
  | Ounce => 0.0283495
  | Pound => 0.453592
  | Stone => 6.35029
  /* Area */
  | Acre => 4047.
  | Hectare => 1e4
  /* Volume */
  | Liter => 1e-3
  | Gallon => 4.54609e-3
  | USGallon => 3.785e-3
  | Quart => 9.464e-4
  | Cup => 2.4e-4
  | USCup => 2.3559e-4
  | Teaspoon => 4.929e-6
  | Tablespoon => 1.479e-5
  | FluidOunce => 2.8413e-5
  /* Speed */
  | Knot => 0.514444
  /* Force */
  | Newton => 1.
  | PoundForce => 4.448222
  /* Pressure */
  | Pascal => 1.
  | Atmosphere => 101325.
  | Bar => 100000.
  /* Energy */
  | Joule => 1.
  | Calorie => 4.184
  | ElectronVolt => 1.602e-19
  | BTU => 1055.
  | Therm => 1055000000.
  /* Power */
  | Watt => 1.
  | Horsepower => 745.7
  | MetricHorsepower => 735.5
  /* Memory */
  | Bit => 1.
  | Byte => 8.
  /* Temperature */
  | Kelvin => 1.
  | Celsius
  | Fahrenheit => assert(false)
  };

let%private celsiusToKelvin = value => Decimal.(value + ofFloat(273.15));
let%private fahrenheitToKelvin = value =>
  Decimal.((value - ofFloat(32.)) / ofFloat(1.8) + ofFloat(273.15));
let%private celsiusFromKelvin = value => Decimal.(value - ofFloat(273.15));
let%private fahrenheitFromKelvin = value =>
  Decimal.((value - ofFloat(273.15)) * ofFloat(1.8) + ofFloat(32.));

let%private unitPartValue = (~powerMultiplier=1, {prefix, unit, power}) => {
  let nextPower = power * powerMultiplier;
  Decimal.(
    (ofFloat(prefixValue(prefix)) * ofFloat(unitLinearValueExn(unit)))
    ** ofInt(nextPower)
  );
};

let%private transformUnits =
            (
              ~transformCelsius,
              ~transformFahrenheit,
              ~powerMultiplier,
              value: Decimal.t,
              units: array(unitPart),
            ) => {
  let handleLinearUnit =
    (. value, unitPart) =>
      switch (unitPart.unit) {
      | Celsius
      | Fahrenheit => Decimal.nan
      | _ => Decimal.(value * unitPartValue(~powerMultiplier, unitPart))
      };

  switch (units) {
  | [|{prefix, unit: Celsius, power: 1}|] =>
    transformCelsius(Decimal.(prefixValue(prefix)->ofFloat * value))
  | [|{prefix, unit: Fahrenheit, power: 1}|] =>
    transformFahrenheit(Decimal.(prefixValue(prefix)->ofFloat * value))
  | _ => Belt.Array.reduceU(units, value, handleLinearUnit)
  };
};

let toSi = (value: Value.t, units) =>
  transformUnits(
    ~transformCelsius=celsiusToKelvin,
    ~transformFahrenheit=fahrenheitToKelvin,
    ~powerMultiplier=1,
    Value.toDecimal(value),
    units,
  )
  ->Value.ofDecimal;

let fromSi = (value: Value.t, units) =>
  transformUnits(
    ~transformCelsius=celsiusFromKelvin,
    ~transformFahrenheit=fahrenheitFromKelvin,
    ~powerMultiplier=-1,
    Value.toDecimal(value),
    units,
  )
  ->Value.ofDecimal;

let convert = (value: Value.t, ~fromUnits, ~toUnits) =>
  if (Units_ConvertChecks.unitsCompatible(~fromUnits, ~toUnits)) {
    value->toSi(fromUnits)->fromSi(toUnits);
  } else {
    `N;
  };

let convertComposite =
    (values: array((Value.t, unitPart)), ~toUnits: array(unitPart)) => {
  let fromUnits = Belt.Array.mapU(values, (. (_, unitPart)) => unitPart);

  if (Units_ConvertChecks.compositeUnitsCompatible(~fromUnits, ~toUnits)) {
    open Decimal;
    let valueSi =
      Belt.Array.reduceU(values, zero, (. accum, (value, unitPart)) =>
        accum
        + Value.toDecimal(value)
        * unitPartValue(~powerMultiplier=1, unitPart)
      );
    let valueSiAbs = abs(valueSi);
    let negative = lt(valueSiAbs, zero);
    let toUnits =
      Belt.Array.copy(toUnits)
      ->ArrayUtil.sortInPlace((a, b) => {
          cmp(unitPartValue(b), unitPartValue(a))
        });
    let remainderSi = ref(valueSiAbs);
    let lastIndex = Pervasives.(Belt.Array.length(toUnits) - 1);
    let output =
      Belt.Array.mapWithIndexU(
        toUnits,
        (. index, unitPart) => {
          let value =
            remainderSi^ * unitPartValue(~powerMultiplier=-1, unitPart);

          let value =
            if (Pervasives.(index == lastIndex)) {
              value;
            } else {
              let valueFloor = floor(value);
              let remainder = value - valueFloor;
              remainderSi :=
                remainder * unitPartValue(~powerMultiplier=1, unitPart);
              valueFloor;
            };
          let value = negative ? Decimal.neg(value) : value;

          switch (toFloat(value)->FloatUtil.asInt) {
          | Some(int) => Value.ofInt(int)
          | None => Value.ofDecimal(value)
          };
        },
      );
    Some(output);
  } else {
    None;
  };
};
