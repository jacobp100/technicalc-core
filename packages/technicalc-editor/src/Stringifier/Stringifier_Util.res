open Stringifier_Types

let decimalSeparator = locale =>
  switch locale {
  | English => "."
  | European => ","
  }

let groupingSeparator = locale =>
  switch locale {
  | English => ","
  | European => "."
  }
