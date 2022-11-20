type expectation<'t>
type testEach<'t>
@val external describe: (string, @uncurry (unit => unit)) => unit = "describe"
@val external test: (string, @uncurry (unit => unit)) => unit = "test"
@val external expect: 't => expectation<'t> = "expect"
@send external toEqual: (expectation<'t>, 't) => unit = "toEqual"
@send external toBe: (expectation<'t>, 't) => unit = "toBe"
@send external toBeCloseTo: (expectation<float>, float) => unit = "toBeCloseTo"
@send external toThrow: (expectation<@uncurry (unit => unit)>, unit) => unit = "toThrow"
