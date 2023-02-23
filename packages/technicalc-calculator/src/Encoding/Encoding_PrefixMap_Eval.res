open Units_Types

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
    | Pebi => 19
    // Set 2 additions
    | Exa => 20
    | Exbi => 21
    }
)

%%private(let numIntElements = 21)
%%private(let maxUintValue = 21)
let mapping = Belt.Array.make(numIntElements + 1, 0)
let reverseMapping = Belt.Array.make(maxUintValue + 1, Unit)
for i in 0 to numIntElements {
  let prefix: prefix = Obj.magic(i)
  let index = toUint(prefix)
  assert Belt.Array.set(mapping, i, index)
  assert Belt.Array.set(reverseMapping, index, prefix)
}
