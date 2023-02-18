open Units_Types

%%private(
  let toUint = (element: name) =>
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
    | Fahrenheit => 57
    /* 2nd additions */
    | ScandinavianMile => 58
    /* 3rd additions */
    | Volt => 59
    | Ampere => 60
    | Ohm => 61
    | Coulomb => 62
    | Farad => 63
    | Weber => 64
    | Tesla => 65
    | Henry => 66
    | Siemens => 67
    | Mole => 68
    | Hertz => 69
    }
)

%%private(let numIntElements = 69)
%%private(let maxUintValue = 69)
let mapping = Belt.Array.make(numIntElements + 1, 0)
let reverseMapping = Belt.Array.make(maxUintValue + 1, Second)
for i in 0 to numIntElements {
  let element: name = Obj.magic(i)
  let index = toUint(element)
  assert Belt.Array.set(mapping, i, index)
  assert Belt.Array.set(reverseMapping, index, element)
}
