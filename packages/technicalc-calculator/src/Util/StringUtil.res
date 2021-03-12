@scope("String") @val external ofChar: int => string = "fromCharCode"
@send external stringCharAtUnsafe: (string, int) => string = "charAt"
@send external charAtUnsafe: (string, int) => int = "charCodeAt"
@send external join: (array<string>, @as("") _) => string = "join"
@send external split: (string, ~separator: string) => array<string> = "split"
@send external replaceFirst: (string, string, string) => string = "replace"
@send external slice: (string, int, int) => string = "slice"
@send external toUpperCase: string => string = "toUpperCase"
@send external repeat: (string, int) => string = "repeat"

external charToInt: char => int = "%identity"

let make = (length, char: char) => ofChar(charToInt(char))->repeat(length)
