@scope("String") @val external ofChar: int => string = "fromCharCode"
@send external stringCharAtUnsafe: (string, int) => string = "charAt"
@send external charAtUnsafe: (string, int) => int = "charCodeAt"
@send external join: (array<string>, @as("") _) => string = "join"
@send external joinWith: (array<string>, string) => string = "join"
@send external split: (string, ~separator: string) => array<string> = "split"
@send external replaceFirst: (string, string, string) => string = "replace"
@send external slice: (string, int, int) => string = "slice"
@send external sliceToEnd: (string, int) => string = "slice"
@send external includes: (string, string) => bool = "includes"
@send external startsWith: (string, string) => bool = "startsWith"
@send external trim: string => string = "trim"
