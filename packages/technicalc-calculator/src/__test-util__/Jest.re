type expectation('t);
type testEach('t);
[@bs.val] external describe: (string, (. unit) => unit) => unit = "describe";
[@bs.val] external test: (string, (. unit) => unit) => unit = "test";
[@bs.val] external expect: 't => expectation('t) = "expect";
[@bs.send] external toEqual: (expectation('t), 't) => unit = "toEqual";
[@bs.send] external toBe: (expectation('t), 't) => unit = "toBe";
