open Jest
open Real

let ofDecimalString = s => ofDecimal(Decimal.ofString(s))

let ofRational = (n, d, c) =>
  switch ofRational(n, d, c) {
  | Rational(_) as r => r
  | Decimal(_) => Js.Exn.raiseError("Not a rational")
  }

test("neg", () => {
  expect(-ofRational(1, 1, Unit))->toEqual(Rational(-1, 1, Unit))
  expect(-ofDecimalString("1"))->toEqual(ofDecimalString("-1"))
})

test("neg with integer overflows", () => {
  expect(-ofRational(lsl(1, 31), 1, Unit))->toEqual(ofDecimalString("2147483648"))
})

test("abs", () => {
  expect(abs(ofRational(-1, 1, Unit)))->toEqual(Rational(1, 1, Unit))
  expect(abs(ofDecimalString("-1")))->toEqual(ofDecimalString("1"))
})

test("abs with integer overflows", () => {
  expect(abs(ofRational(lsl(1, 31), 1, Unit)))->toEqual(ofDecimalString("2147483648"))
})

test("add", () => {
  expect(ofRational(1, 1, Unit) + ofRational(1, 1, Unit))->toEqual(Rational(2, 1, Unit))
  expect(ofRational(1, 1, Unit) + ofDecimalString("1"))->toEqual(ofDecimalString("2"))
  expect(ofDecimalString("1") + ofDecimalString("1"))->toEqual(ofDecimalString("2"))
})

test("add constants of equal type", () => {
  expect(ofRational(1, 1, Pi(1)) + ofRational(1, 1, Pi(1)))->toEqual(Rational(2, 1, Pi(1)))
  expect(ofRational(1, 1, Pi(2)) + ofRational(1, 1, Pi(2)))->toEqual(Rational(2, 1, Pi(2)))
  expect(ofRational(1, 1, Sqrt(2)) + ofRational(1, 1, Sqrt(2)))->toEqual(Rational(2, 1, Sqrt(2)))

  expect(ofRational(1, 4, Pi(1)) + ofRational(3, 4, Pi(1)))->toEqual(Rational(1, 1, Pi(1)))

  expect(ofRational(1, 1, Pi(1)) + ofRational(1, 1, Pi(2)))->toEqual(
    ofDecimalString("13.011197054679151858"),
  )
  expect(ofRational(1, 1, Sqrt(2)) + ofRational(1, 1, Sqrt(3)))->toEqual(
    ofDecimalString("3.1462643699419723423"),
  )
})

test("add constants of differing type", () => {
  expect(ofRational(1, 1, Pi(1)) + ofRational(1, 1, Unit))->toEqual(
    ofDecimalString("4.1415926535897932385"),
  )
})

test("add with integer overflows", () => {
  expect(ofRational(lsl(1, 30), 1, Unit) + ofRational(lsl(1, 30), 1, Unit))->toEqual(
    ofDecimalString("2147483648"),
  )
  expect(ofRational(lsl(1, 30), 1, Pi(1)) + ofRational(lsl(1, 30), 1, Pi(1)))->toEqual(
    ofDecimalString("6746518852.2610094794"),
  )
})

test("sub", () => {
  expect(ofRational(3, 1, Unit) - ofRational(1, 1, Unit))->toEqual(Rational(2, 1, Unit))
  expect(ofRational(3, 1, Unit) - ofDecimalString("1"))->toEqual(ofDecimalString("2"))
  expect(ofDecimalString("3") - ofDecimalString("1"))->toEqual(ofDecimalString("2"))
})

test("sub constants of equal type", () => {
  expect(ofRational(3, 1, Pi(1)) - ofRational(1, 1, Pi(1)))->toEqual(Rational(2, 1, Pi(1)))
  expect(ofRational(3, 1, Pi(2)) - ofRational(1, 1, Pi(2)))->toEqual(Rational(2, 1, Pi(2)))
  expect(ofRational(3, 1, Sqrt(2)) - ofRational(1, 1, Sqrt(2)))->toEqual(Rational(2, 1, Sqrt(2)))

  expect(ofRational(5, 4, Pi(1)) - ofRational(1, 4, Pi(1)))->toEqual(Rational(1, 1, Pi(1)))

  expect(ofRational(1, 1, Pi(1)) - ofRational(1, 1, Pi(2)))->toEqual(
    ofDecimalString("-6.7280117474995653806"),
  )
  expect(ofRational(1, 1, Sqrt(2)) - ofRational(1, 1, Sqrt(3)))->toEqual(
    ofDecimalString("-0.3178372451957822447"),
  )
})

test("sub constants of differing type", () => {
  expect(ofRational(1, 1, Pi(1)) - ofRational(1, 1, Unit))->toEqual(
    ofDecimalString("2.1415926535897932385"),
  )
})

test("sub with integer overflows", () => {
  expect(ofRational(lsl(1, 31), 1, Unit) - ofRational(lsl(1, 30), 1, Unit))->toEqual(
    ofDecimalString("-3221225472"),
  )
  expect(ofRational(lsl(1, 31), 1, Pi(1)) - ofRational(lsl(1, 30), 1, Pi(1)))->toEqual(
    ofDecimalString("-10119778278.391514219"),
  )
})

test("mul", () => {
  expect(ofRational(1, 1, Pi(1)) * ofRational(1, 1, Unit))->toEqual(Rational(1, 1, Pi(1)))
  expect(ofRational(1, 1, Unit) * ofRational(1, 1, Pi(1)))->toEqual(Rational(1, 1, Pi(1)))
  expect(ofRational(1, 1, Exp(1)) * ofRational(1, 1, Unit))->toEqual(Rational(1, 1, Exp(1)))
  expect(ofRational(1, 1, Unit) * ofRational(1, 1, Exp(1)))->toEqual(Rational(1, 1, Exp(1)))
  expect(ofRational(1, 1, Sqrt(2)) * ofRational(1, 1, Unit))->toEqual(Rational(1, 1, Sqrt(2)))
  expect(ofRational(1, 1, Unit) * ofRational(1, 1, Sqrt(2)))->toEqual(Rational(1, 1, Sqrt(2)))

  expect(ofRational(1, 1, Unit) * ofDecimalString("1"))->toEqual(ofDecimalString("1"))
  expect(ofDecimalString("1") * ofRational(1, 1, Unit))->toEqual(ofDecimalString("1"))
  expect(ofRational(1, 1, Pi(1)) * ofDecimalString("1"))->toEqual(ofDecimal(Decimal.pi))
  expect(ofRational(1, 1, Exp(1)) * ofDecimalString("1"))->toEqual(
    ofDecimal(Decimal.exp(Decimal.one)),
  )
  expect(ofRational(1, 1, Sqrt(2)) * ofDecimalString("1"))->toEqual(
    ofDecimal(Decimal.sqrt(Decimal.ofInt(2))),
  )
})

test("mul with constants based on exponents", () => {
  expect(ofRational(1, 1, Pi(1)) * ofRational(2, 1, Pi(1)))->toEqual(Rational(2, 1, Pi(2)))
  expect(ofRational(1, 1, Exp(1)) * ofRational(2, 1, Exp(1)))->toEqual(Rational(2, 1, Exp(2)))
})

test("mul with differing constants", () => {
  expect(ofRational(1, 1, Pi(1)) * ofRational(1, 1, Exp(1)))->toEqual(
    ofDecimalString("8.5397342226735670657"),
  )
  expect(ofRational(1, 1, Pi(1)) * ofRational(1, 1, Sqrt(2)))->toEqual(
    ofDecimalString("4.4428829381583662471"),
  )
  expect(ofRational(1, 1, Exp(1)) * ofRational(1, 1, Pi(1)))->toEqual(
    ofDecimalString("8.5397342226735670657"),
  )
  expect(ofRational(1, 1, Exp(1)) * ofRational(1, 1, Sqrt(2)))->toEqual(
    ofDecimalString("3.8442310281591168249"),
  )
  expect(ofRational(1, 1, Sqrt(2)) * ofRational(1, 1, Pi(1)))->toEqual(
    ofDecimalString("4.4428829381583662471"),
  )
  expect(ofRational(1, 1, Sqrt(2)) * ofRational(1, 1, Exp(1)))->toEqual(
    ofDecimalString("3.8442310281591168249"),
  )
})

test("mul with constant-based integer overflows", () => {
  expect(ofRational(1, 1, Pi(lsl(1, 30))) * ofRational(1, 1, Pi(lsl(1, 30))))->toEqual(
    ofDecimalString("1.644122443231386251e+1067621222"),
  )
  expect(ofRational(1, 1, Exp(lsl(1, 30))) * ofRational(1, 1, Exp(lsl(1, 30))))->toEqual(
    ofDecimalString("2.0130993930689639565e+932640298"),
  )
  expect(ofRational(1, 1, Sqrt(lsl(1, 30))) * ofRational(1, 1, Sqrt(lsl(1, 30))))->toEqual(
    Rational(1073741824, 1, Unit),
  )
})

test("mul with differing constants", () => {
  expect(ofRational(1, 1, Pi(1)) * ofRational(2, 1, Sqrt(2)))->toEqual(
    ofDecimalString("8.8857658763167324941"),
  )
  expect(ofRational(1, 1, Sqrt(2)) * ofRational(2, 1, Pi(1)))->toEqual(
    ofDecimalString("8.8857658763167324941"),
  )
  expect(ofRational(1, 1, Exp(1)) * ofRational(2, 1, Sqrt(2)))->toEqual(
    ofDecimalString("7.6884620563182336498"),
  )
  expect(ofRational(2, 1, Sqrt(2)) * ofRational(1, 1, Exp(1)))->toEqual(
    ofDecimalString("7.6884620563182336498"),
  )
})

test("mul with constants based on square roots", () => {
  expect(ofRational(1, 1, Sqrt(2)) * ofRational(2, 1, Sqrt(3)))->toEqual(Rational(2, 1, Sqrt(6)))
})

test("div", () => {
  expect(ofRational(1, 3, Unit) / ofRational(2, 3, Unit))->toEqual(Rational(1, 2, Unit))
  expect(ofRational(1, 1, Unit) / ofRational(0, 0, Unit))->toEqual(Rational(1, 0, Unit))

  expect(ofRational(1, 1, Unit) / ofDecimalString("1"))->toEqual(ofDecimalString("1"))
  expect(ofDecimalString("1") / ofRational(1, 1, Unit))->toEqual(ofDecimalString("1"))

  expect(ofRational(1, 1, Unit) / ofRational(1, 1, Pi(1)))->toEqual(Rational(1, 1, Pi(-1)))

  expect(ofRational(1, 1, Pi(1)) / ofDecimalString("1"))->toEqual(
    ofDecimalString("3.1415926535897932385"),
  )
})

test("div with constants on lhs", () => {
  expect(ofRational(1, 3, Pi(1)) / ofRational(2, 3, Unit))->toEqual(Rational(1, 2, Pi(1)))
  expect(ofRational(1, 3, Exp(1)) / ofRational(2, 3, Unit))->toEqual(Rational(1, 2, Exp(1)))
  expect(ofRational(1, 3, Sqrt(2)) / ofRational(2, 3, Unit))->toEqual(Rational(1, 2, Sqrt(2)))
})

test("div with constants on rhs", () => {
  expect(ofRational(2, 1, Unit) / ofRational(1, 1, Sqrt(2)))->toEqual(Rational(1, 1, Sqrt(2)))
})

test("div with matching constants", () => {
  expect(ofRational(1, 3, Pi(1)) / ofRational(2, 3, Pi(1)))->toEqual(Rational(1, 2, Unit))
  expect(ofRational(1, 3, Exp(1)) / ofRational(2, 3, Exp(1)))->toEqual(Rational(1, 2, Unit))
  expect(ofRational(1, 3, Sqrt(2)) / ofRational(2, 3, Sqrt(2)))->toEqual(Rational(1, 2, Unit))
})

test("div with constants based on exponents", () => {
  expect(ofRational(1, 3, Pi(2)) / ofRational(2, 3, Pi(1)))->toEqual(Rational(1, 2, Pi(1)))
  expect(ofRational(1, 3, Exp(2)) / ofRational(2, 3, Exp(1)))->toEqual(Rational(1, 2, Exp(1)))
  expect(ofRational(1, 3, Unit) / ofRational(2, 3, Pi(1)))->toEqual(Rational(1, 2, Pi(-1)))
  expect(ofRational(1, 3, Unit) / ofRational(2, 3, Exp(1)))->toEqual(Rational(1, 2, Exp(-1)))

  expect(ofRational(1, 2, Exp(1)) / ofRational(1, 1, Pi(1)))->toEqual(
    ofDecimalString("0.4326279897161325436"),
  )
})

test("div with constant-based integer overflows", () => {
  expect(ofRational(1, 1, Pi(lsl(1, 31))) / ofRational(1, 1, Pi(lsl(1, 30))))->toEqual(
    ofDecimalString("4.7434984539777399675e-1601431834"),
  )
  expect(ofRational(1, 1, Exp(lsl(1, 31))) / ofRational(1, 1, Exp(lsl(1, 30))))->toEqual(
    ofDecimalString("3.5010811172673073127e-1398960448"),
  )
})

test("div with sqrt", () => {
  expect(ofRational(1, 1, Sqrt(6)) / ofRational(1, 1, Sqrt(3)))->toEqual(Rational(1, 1, Sqrt(2)))
  expect(ofRational(1, 1, Sqrt(2)) / ofRational(1, 1, Sqrt(0)))->toEqual(Rational(1, 0, Unit))
})

test("div with differing constants", () => {
  expect(ofRational(1, 1, Pi(1)) / ofRational(1, 1, Exp(1)))->toEqual(
    ofDecimalString("1.1557273497909217179"),
  )
  expect(ofRational(1, 1, Pi(1)) / ofRational(1, 1, Sqrt(2)))->toEqual(
    ofDecimalString("2.2214414690791831235"),
  )
  expect(ofRational(1, 1, Exp(1)) / ofRational(1, 1, Pi(1)))->toEqual(
    ofDecimalString("0.86525597943226508721"),
  )
  expect(ofRational(1, 1, Exp(1)) / ofRational(1, 1, Sqrt(2)))->toEqual(
    ofDecimalString("1.9221155140795584125"),
  )
  expect(ofRational(1, 1, Sqrt(2)) / ofRational(1, 1, Pi(1)))->toEqual(
    ofDecimalString("0.45015815807855303477"),
  )
  expect(ofRational(1, 1, Sqrt(2)) / ofRational(1, 1, Exp(1)))->toEqual(
    ofDecimalString("0.52026009502288889636"),
  )
})

test("powInt", () => {
  expect(powInt(ofRational(0, 1, Unit), 0))->toEqual(nan)
  expect(powInt(ofRational(2, 1, Unit), 0))->toEqual(Rational(1, 1, Unit))
  expect(powInt(ofRational(2, 1, Unit), 1))->toEqual(Rational(2, 1, Unit))
  expect(powInt(ofRational(2, 1, Unit), 2))->toEqual(Rational(4, 1, Unit))
  expect(powInt(ofRational(2, 1, Unit), 3))->toEqual(Rational(8, 1, Unit))

  expect(powInt(ofRational(2, 1, Unit), -1))->toEqual(Rational(1, 2, Unit))
  expect(powInt(ofRational(2, 1, Unit), -2))->toEqual(Rational(1, 4, Unit))
  expect(powInt(ofRational(2, 1, Unit), -3))->toEqual(Rational(1, 8, Unit))

  expect(powInt(ofDecimalString("2"), 3))->toEqual(ofDecimalString("8"))
})

test("powInt with constants", () => {
  expect(powInt(ofRational(2, 1, Pi(1)), 1))->toEqual(Rational(2, 1, Pi(1)))
  expect(powInt(ofRational(2, 1, Pi(1)), 2))->toEqual(Rational(4, 1, Pi(2)))
  expect(powInt(ofRational(2, 1, Pi(1)), 3))->toEqual(Rational(8, 1, Pi(3)))

  expect(powInt(ofRational(2, 1, Pi(1)), -1))->toEqual(Rational(1, 2, Pi(-1)))
  expect(powInt(ofRational(2, 1, Pi(1)), -2))->toEqual(Rational(1, 4, Pi(-2)))
  expect(powInt(ofRational(2, 1, Pi(1)), -3))->toEqual(Rational(1, 8, Pi(-3)))

  expect(powInt(ofRational(2, 1, Exp(1)), 1))->toEqual(Rational(2, 1, Exp(1)))
  expect(powInt(ofRational(2, 1, Exp(1)), 2))->toEqual(Rational(4, 1, Exp(2)))
  expect(powInt(ofRational(2, 1, Exp(1)), 3))->toEqual(Rational(8, 1, Exp(3)))

  expect(powInt(ofRational(2, 1, Exp(1)), -1))->toEqual(Rational(1, 2, Exp(-1)))
  expect(powInt(ofRational(2, 1, Exp(1)), -2))->toEqual(Rational(1, 4, Exp(-2)))
  expect(powInt(ofRational(2, 1, Exp(1)), -3))->toEqual(Rational(1, 8, Exp(-3)))

  expect(powInt(ofRational(2, 1, Sqrt(2)), 1))->toEqual(Rational(2, 1, Sqrt(2)))
  expect(powInt(ofRational(2, 1, Sqrt(2)), 2))->toEqual(Rational(8, 1, Unit))
  expect(powInt(ofRational(2, 1, Sqrt(2)), 3))->toEqual(Rational(16, 1, Sqrt(2)))

  expect(powInt(ofRational(2, 1, Sqrt(2)), -1))->toEqual(Rational(1, 4, Sqrt(2)))
  expect(powInt(ofRational(2, 1, Sqrt(2)), -2))->toEqual(Rational(1, 8, Unit))
  expect(powInt(ofRational(2, 1, Sqrt(2)), -3))->toEqual(Rational(1, 32, Sqrt(2)))
})

test("powInt with integer overflows", () => {
  expect(powInt(ofRational(lsl(1, 30), 1, Unit), 2))->toEqual(
    ofDecimalString("1152921504606846976"),
  )
  expect(powInt(ofRational(lsl(1, 30), 1, Unit), 3))->toEqual(
    ofDecimalString("1.2379400392853802749e+27"),
  )

  expect(powInt(ofRational(lsl(1, 30), 1, Unit), -2))->toEqual(
    ofDecimalString("8.6736173798840354721e-19"),
  )
  expect(powInt(ofRational(lsl(1, 30), 1, Unit), -3))->toEqual(
    ofDecimalString("8.0779356694631608874e-28"),
  )
})

test("powInt with constant-based integer overflows", () => {
  let largeNumberNotSimplifiable = {
    open Belt.Int
    lsl(1, 30) - 5
  }

  expect(powInt(ofRational(1, 1, Pi(lsl(1, 31))), -1))->toEqual(
    ofDecimalString("1.644122443231386251e+1067621222"),
  )
  expect(powInt(ofRational(1, 1, Exp(lsl(1, 31))), -1))->toEqual(
    ofDecimalString("2.0130993930689639563e+932640298"),
  )
  expect(powInt(ofRational(lsl(1, 30), 1, Sqrt(largeNumberNotSimplifiable)), -1))->toEqual(
    ofDecimalString("2.842170949657845667e-14"),
  )

  expect(powInt(ofRational(1, 1, Pi(lsl(1, 31))), -3))->toEqual(
    ofDecimalString("4.4442908531323526699e+3202863666"),
  )
  expect(powInt(ofRational(1, 1, Exp(lsl(1, 31))), -3))->toEqual(
    ofDecimalString("8.1582245291987669729e+2797920894"),
  )
  expect(powInt(ofRational(lsl(1, 30), 1, Sqrt(largeNumberNotSimplifiable)), -3))->toEqual(
    ofDecimalString("2.2958874199863686428e-41"),
  )
})

test("pow with precision loss", () => {
  expect(pow(ofInt(64), ofRational(1, 3, Unit)))->toEqual(Rational(4, 1, Unit))
})
