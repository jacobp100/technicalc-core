@scope("String") @val external ofChar: int => string = "fromCharCode"
@send external stringCharAtUnsafe: (string, int) => string = "charAt"
@send external charAtUnsafe: (string, int) => int = "charCodeAt"
@send external join: (array<string>, @as("") _) => string = "join"
