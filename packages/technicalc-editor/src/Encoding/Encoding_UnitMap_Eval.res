open TechniCalcCalculator.Unit_Types

%%private(let makeSureThisIsTheLastIndex = 52)
%%private(
  let toUint = (element: unitType) =>
    switch element {
    | Second => 0
    | Minute => 1
    | Hour => 2
    | Day => 3
    | Week => 4
    | Month => 5
    | Year => 6
    | Decade => 7
    | Century => 8
    | Meter => 9
    | Inch => 10
    | Foot => 11
    | Yard => 12
    | Mile => 13
    | NauticalMile => 14
    | LightYear => 15
    | Parsec => 16
    | Angstrom => 17
    | Gram => 18
    | Tonne => 19
    | Ounce => 20
    | Pound => 21
    | Stone => 22
    | Acre => 23
    | Hectare => 24
    | Liter => 25
    | Gallon => 26
    | USGallon => 27
    | Quart => 28
    | Cup => 29
    | USCup => 30
    | Teaspoon => 31
    | Tablespoon => 32
    | FluidOunce => 33
    | Knot => 34
    | Newton => 35
    | PoundForce => 36
    | Pascal => 37
    | Atmosphere => 38
    | Bar => 39
    | Joule => 40
    | Calorie => 41
    | ElectronVolt => 42
    | BTU => 43
    | Therm => 44
    | Watt => 45
    | Horsepower => 46
    | MetricHorsepower => 47
    | Bit => 48
    | Byte => 49
    | Kelvin => 50
    | Celsius => 51
    | Fahrenheit => makeSureThisIsTheLastIndex
    }
)

let mapping = Belt.Array.make(makeSureThisIsTheLastIndex + 1, 0)
let reverseMapping = Belt.Array.make(makeSureThisIsTheLastIndex + 1, Second)
for i in 0 to makeSureThisIsTheLastIndex {
  let unit: unitType = Obj.magic(i)
  let index = toUint(unit)
  assert Belt.Array.set(mapping, i, index)
  assert Belt.Array.set(reverseMapping, index, unit)
}
