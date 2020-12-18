[@bs.scope "String"] [@bs.val] external ofChar: int => string = "fromCharCode";
[@bs.send] external stringCharAtUnsafe: (string, int) => string = "charAt";
[@bs.send] external charAtUnsafe: (string, int) => int = "charCodeAt";
[@bs.send] external join: (array(string), [@bs.as ""] _) => string = "join";
[@bs.send]
external split: (string, ~separator: string) => array(string) = "split";
[@bs.send]
external replaceFirst: (string, string, string) => string = "replace";
[@bs.send] external slice: (string, int, int) => string = "slice";
[@bs.send] external toUpperCase: string => string = "toUpperCase";
[@bs.send] external repeat: (string, int) => string = "repeat";

external charToInt: char => int = "%identity";

let make = (length, char: char) => ofChar(charToInt(char))->repeat(length);
