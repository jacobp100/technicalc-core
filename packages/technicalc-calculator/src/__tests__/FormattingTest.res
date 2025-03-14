open Jest
open Value
open Formatting

let stringOfFloat = (x, format) => toString(~format, ofFloat(x))

test("formats fractions", () => {
  let format = {...defaultFormat, fractions: Improper}

  expect(stringOfFloat(1.5, format))->toBe("3/2")
  expect(stringOfFloat(-1.5, format))->toBe("-3/2")

  expect(toString(~format, pi))->toBe("pi")
  expect(toString(~format, ofInt(4) * pi))->toBe("4pi")
  expect(toString(~format, ofInt(-4) * pi))->toBe("-4pi")
  expect(toString(~format, ofInt(4) * pi / ofInt(3)))->toBe("4pi/3")
  expect(toString(~format, ofInt(-4) * pi / ofInt(3)))->toBe("-4pi/3")
})

test("formats mixed fractions", () => {
  let format = {...defaultFormat, fractions: Mixed}

  expect(stringOfFloat(1.5, format))->toBe("1+1/2")
  expect(stringOfFloat(-1.5, format))->toBe("-(1+1/2)")

  expect(toString(~format, pi))->toBe("pi")
  expect(toString(~format, ofInt(4) * pi))->toBe("4pi")
  expect(toString(~format, ofInt(-4) * pi))->toBe("-4pi")
  expect(toString(~format, ofInt(4) * pi / ofInt(3)))->toBe("4pi/3")
  expect(toString(~format, ofInt(-4) * pi / ofInt(3)))->toBe("-4pi/3")
})

test("formats fractions in other bases", () => {
  expect(stringOfFloat(1.5, {...defaultFormat, fractions: Improper, base: 2}))->toBe("0b11/0b10")
})

test("formats mixed fractions in other bases", () => {
  expect(stringOfFloat(1.5, {...defaultFormat, fractions: Mixed, base: 2}))->toBe("0b1+0b1/0b10")
})

test("only adds commas for values greater than 999", () => {
  let format = {...defaultFormat, fractions: Never}

  expect(stringOfFloat(100000000., format))->toBe("100,000,000")
  expect(stringOfFloat(10000000., format))->toBe("10,000,000")
  expect(stringOfFloat(1000000., format))->toBe("1,000,000")
  expect(stringOfFloat(100000., format))->toBe("100,000")
  expect(stringOfFloat(10000., format))->toBe("10,000")
  expect(stringOfFloat(1000., format))->toBe("1,000")
})

test("formats magnitude of reals", () => {
  let format = {...defaultFormat, fractions: Never}

  expect(stringOfFloat(1000000000., format))->toBe("1e9")
  expect(stringOfFloat(100., format))->toBe("100")
  expect(stringOfFloat(10., format))->toBe("10")
  expect(stringOfFloat(1., format))->toBe("1")
  expect(stringOfFloat(0.1, format))->toBe("0.1")
  expect(stringOfFloat(0.01, format))->toBe("0.01")
  expect(stringOfFloat(0.001, format))->toBe("0.001")
  expect(stringOfFloat(0.0001, format))->toBe("1e-4")
})

test("trims trailling zeros", () => {
  let format = {...defaultFormat, fractions: Never}

  expect(stringOfFloat(1.23e9, format))->toBe("1.23e9")
  expect(stringOfFloat(1.23, format))->toBe("1.23")
  expect(stringOfFloat(1.23e-4, format))->toBe("1.23e-4")
  expect(stringOfFloat(1.234567890123456789e9, format))->toBe("1.234567890123e9")
  expect(stringOfFloat(1.234567890123456789, format))->toBe("1.234567890123")
  expect(stringOfFloat(1.234567890123456789e-4, format))->toBe("1.234567890123e-4")
  expect(stringOfFloat(1.0000000000001, format))->toBe("1")
})

test("formats engineering notation", () => {
  let format = {
    ...defaultFormat,
    decimalMinMagnitude: 0,
    decimalMaxMagnitude: 0,
    minDecimalPlaces: 12,
    maxDecimalPlaces: 12,
    exponents: Engineering,
  }

  expect(stringOfFloat(1., format))->toBe("1.000000000000e0")
  expect(stringOfFloat(10., format))->toBe("10.000000000000e0")
  expect(stringOfFloat(100., format))->toBe("100.000000000000e0")
  expect(stringOfFloat(1000., format))->toBe("1.000000000000e3")
  expect(stringOfFloat(-1., format))->toBe("-1.000000000000e0")
  expect(stringOfFloat(-10., format))->toBe("-10.000000000000e0")
  expect(stringOfFloat(-100., format))->toBe("-100.000000000000e0")
  expect(stringOfFloat(-1000., format))->toBe("-1.000000000000e3")

  expect(stringOfFloat(0.00005, format))->toBe("50.000000000000e-6")
  expect(stringOfFloat(0.0005, format))->toBe("500.000000000000e-6")
  expect(stringOfFloat(0.005, format))->toBe("5.000000000000e-3")
  expect(stringOfFloat(0.05, format))->toBe("50.000000000000e-3")
  expect(stringOfFloat(0.5, format))->toBe("500.000000000000e-3")
  expect(stringOfFloat(-0.00005, format))->toBe("-50.000000000000e-6")
  expect(stringOfFloat(-0.0005, format))->toBe("-500.000000000000e-6")
  expect(stringOfFloat(-0.005, format))->toBe("-5.000000000000e-3")
  expect(stringOfFloat(-0.05, format))->toBe("-50.000000000000e-3")
  expect(stringOfFloat(-0.5, format))->toBe("-500.000000000000e-3")
})

test("formats numbers where the exponent is larger than a 32 bit integer", () => {
  let value = ofInt(1000)->pow(ofInt(1000))->pow(ofInt(1000))->pow(ofInt(1000))
  expect(toString(~format=defaultFormat, value))->toBe("1e3000000000")
})

test("formats other bases", () => {
  let format = base => {...defaultFormat, base}

  expect(stringOfFloat(100., format(2)))->toBe("0b110,0100")
  expect(stringOfFloat(100., format(8)))->toBe("0o144")
  expect(stringOfFloat(100., format(16)))->toBe("0x64")
  expect(stringOfFloat(-100., format(2)))->toBe("-0b110,0100")
  expect(stringOfFloat(-100., format(8)))->toBe("-0o144")
  expect(stringOfFloat(-100., format(16)))->toBe("-0x64")
})

test("formats various numbers correctly", () => {
  let convert = x => ofString(x)->Belt.Option.getExn->(toString(~format=defaultFormat, _))

  expect(convert("46.47897327055571"))->toBe("46.478973270556")
  expect(convert("-47.86759243619015"))->toBe("-47.86759243619")
  expect(convert("7.712346515387281"))->toBe("7.712346515387")
  expect(convert("-41.08525582534328"))->toBe("-41.085255825343")
  expect(convert("24.036159870635387"))->toBe("24.036159870635")
  expect(convert("21.622267655248706"))->toBe("21.622267655249")
  expect(convert("85.87032800784263"))->toBe("85.870328007843")
  expect(convert("6.759552690635729"))->toBe("6.759552690636")
  expect(convert("17.724509834485048"))->toBe("17.724509834485")
  expect(convert("-17.618661853244163"))->toBe("-17.618661853244")
  expect(convert("-71.09059285654436"))->toBe("-71.090592856544")
  expect(convert("47.438865505981084"))->toBe("47.438865505981")
  expect(convert("-18.28378337248739"))->toBe("-18.283783372487")
  expect(convert("71.18764618368766"))->toBe("71.187646183688")
  expect(convert("-66.96121712260108"))->toBe("-66.961217122601")
  expect(convert("28.50749266445851"))->toBe("28.507492664459")
  expect(convert("16.454703415645668"))->toBe("16.454703415646")
  expect(convert("-64.43380990868866"))->toBe("-64.433809908689")
  expect(convert("-66.90487607393479"))->toBe("-66.904876073935")
  expect(convert("-28.212396089967342"))->toBe("-28.212396089967")
})

test("formatting decimal number between -1 and 0", () => {
  let aDecimalNumber = log(one / ofInt(3)) / ofInt(4)

  expect(toString(~format=defaultFormat, aDecimalNumber))->toBe("-0.274653072167")
})

test("formats 0.9r", () => {
  expect(
    toString(
      ~format=Formatting.defaultFormat,
      ofDecimal(Decimal.ofString("0.99999999999999999999")),
    ),
  )->toBe("1")
  expect(
    toString(
      ~format=Formatting.defaultFormat,
      ofDecimal(Decimal.ofString("1.99999999999999999999")),
    ),
  )->toBe("2")
})
