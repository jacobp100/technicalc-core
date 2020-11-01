[@bs.scope "String"] [@bs.val] external ofChar: int => string = "fromCharCode";
[@bs.send] external stringCharAtUnsafe: (string, int) => string = "charAt";
[@bs.send] external charAtUnsafe: (string, int) => int = "charCodeAt";
[@bs.send] external join: (array(string), [@bs.as ""] _) => string = "join";
[@bs.send]
external split: (string, ~separator: string) => array(string) = "split";
[@bs.send]
external replaceFirst: (string, string, string) => string = "replace";
[@bs.send] external slice: (string, int, int) => string = "slice";

external charToInt: char => int = "%identity";

let rec splitOnChar = (value, character) =>
  switch (
    Js.String.splitAtMost(value, ~limit=1, charToInt(character)->ofChar)
  ) {
  | [|head, tail|] => [head, ...splitOnChar(tail, character)]
  | _ => [value]
  };
