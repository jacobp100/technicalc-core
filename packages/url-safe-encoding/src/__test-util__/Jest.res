type expectation<'t>
type testEach<'t>
@val external describe: (string, (. unit) => unit) => unit = "describe"
@val external test: (string, (. unit) => unit) => unit = "test"
@val external expect: 't => expectation<'t> = "expect"
@send external toEqual: (expectation<'t>, 't) => unit = "toEqual"
@send external toBe: (expectation<'t>, 't) => unit = "toBe"
@send external toThrow: (expectation<(. unit) => unit>, unit) => unit = "toThrow"
