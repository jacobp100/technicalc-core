open TechniCalcCalculator.Unit_Types

%%private(let makeSureThisIsTheLastIndex = 19)
%%private(
  let toUint = (element: prefix) =>
    switch element {
    | Femto => 0
    | Pico => 1
    | Nano => 2
    | Micro => 3
    | Milli => 4
    | Centi => 5
    | Deci => 6
    | Unit => 7
    | Deca => 8
    | Hecto => 9
    | Kilo => 10
    | Mega => 11
    | Giga => 12
    | Tera => 13
    | Peta => 14
    | Kibi => 15
    | Mebi => 16
    | Gibi => 17
    | Tebi => 18
    | Pebi => makeSureThisIsTheLastIndex
    }
)

let mapping = Belt.Array.make(makeSureThisIsTheLastIndex + 1, 0)
let reverseMapping = Belt.Array.make(makeSureThisIsTheLastIndex + 1, Kilo)
for i in 0 to makeSureThisIsTheLastIndex {
  let prefix: prefix = Obj.magic(i)
  let index = toUint(prefix)
  assert Belt.Array.set(mapping, i, index)
  assert Belt.Array.set(reverseMapping, index, prefix)
}
