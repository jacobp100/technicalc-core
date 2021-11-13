open TechniCalcCalculator.Unit_Types

%%private(let makeSureThisIsTheLastIndex = 57)
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
    | USQuart => 29
    | Cup => 30
    | USCup => 31
    | Pint => 32
    | USPint => 33
    | Teaspoon => 34
    | USTeaspoon => 35
    | Tablespoon => 36
    | USTablespoon => 37
    | FluidOunce => 38
    | Knot => 39
    | Newton => 40
    | PoundForce => 41
    | Pascal => 42
    | Atmosphere => 43
    | Bar => 44
    | Joule => 45
    | Calorie => 46
    | ElectronVolt => 47
    | BTU => 48
    | Therm => 49
    | Watt => 50
    | Horsepower => 51
    | MetricHorsepower => 52
    | Bit => 53
    | Byte => 54
    | Kelvin => 55
    | Celsius => 56
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
