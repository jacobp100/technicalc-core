open Jest
open Real

let ofDecimalString = s => ofDecimal(Decimal.ofString(s))

let ofRational = (n, d, c) =>
  switch ofRational(n, d, c) {
  | Rational(_) as r => r
  | Decimal(_) => Js.Exn.raiseError("Not a rational")
  }

test("neg", (. ()) => {
  expect(-ofRational(1, 1, Unit))->toEqual(Rational(-1, 1, Unit))
  expect(-ofDecimalString("1"))->toEqual(ofDecimalString("-1"))
})

test("neg with integer overflows", (. ()) => {
  expect(-ofRational(lsl(1, 31), 1, Unit))->toEqual(ofDecimalString("2147483648"))
})

test("abs", (. ()) => {
  expect(abs(ofRational(-1, 1, Unit)))->toEqual(Rational(1, 1, Unit))
  expect(abs(ofDecimalString("-1")))->toEqual(ofDecimalString("1"))
})

test("abs with integer overflows", (. ()) => {
  expect(abs(ofRational(lsl(1, 31), 1, Unit)))->toEqual(ofDecimalString("2147483648"))
})

test("add", (. ()) => {
  expect(ofRational(1, 1, Unit) + ofRational(1, 1, Unit))->toEqual(Rational(2, 1, Unit))
  expect(ofRational(1, 1, Unit) + ofDecimalString("1"))->toEqual(ofDecimalString("2"))
  expect(ofDecimalString("1") + ofDecimalString("1"))->toEqual(ofDecimalString("2"))
})

test("add constants of equal type", (. ()) => {
  expect(ofRational(1, 1, Pi(1)) + ofRational(1, 1, Pi(1)))->toEqual(Rational(2, 1, Pi(1)))
  expect(ofRational(1, 1, Pi(2)) + ofRational(1, 1, Pi(2)))->toEqual(Rational(2, 1, Pi(2)))
  expect(ofRational(1, 1, Sqrt(2)) + ofRational(1, 1, Sqrt(2)))->toEqual(Rational(2, 1, Sqrt(2)))

  expect(ofRational(1, 4, Pi(1)) + ofRational(3, 4, Pi(1)))->toEqual(Rational(1, 1, Pi(1)))

  expect(ofRational(1, 1, Pi(1)) + ofRational(1, 1, Pi(2)))->toEqual(
    ofDecimalString("13.01119705467915185729713438315566"),
  )
  expect(ofRational(1, 1, Sqrt(2)) + ofRational(1, 1, Sqrt(3)))->toEqual(
    ofDecimalString("3.14626436994197234232913506571557"),
  )
})

test("add constants of differing type", (. ()) => {
  expect(ofRational(1, 1, Pi(1)) + ofRational(1, 1, Unit))->toEqual(
    ofDecimalString("4.141592653589793238462643383279503"),
  )
})

test("add with integer overflows", (. ()) => {
  expect(ofRational(lsl(1, 30), 1, Unit) + ofRational(lsl(1, 30), 1, Unit))->toEqual(
    ofDecimalString("2147483648"),
  )
  expect(ofRational(lsl(1, 30), 1, Pi(1)) + ofRational(lsl(1, 30), 1, Pi(1)))->toEqual(
    ofDecimalString("6746518852.26100947929949132444813"),
  )
})

test("sub", (. ()) => {
  expect(ofRational(3, 1, Unit) - ofRational(1, 1, Unit))->toEqual(Rational(2, 1, Unit))
  expect(ofRational(3, 1, Unit) - ofDecimalString("1"))->toEqual(ofDecimalString("2"))
  expect(ofDecimalString("3") - ofDecimalString("1"))->toEqual(ofDecimalString("2"))
})

test("sub constants of equal type", (. ()) => {
  expect(ofRational(3, 1, Pi(1)) - ofRational(1, 1, Pi(1)))->toEqual(Rational(2, 1, Pi(1)))
  expect(ofRational(3, 1, Pi(2)) - ofRational(1, 1, Pi(2)))->toEqual(Rational(2, 1, Pi(2)))
  expect(ofRational(3, 1, Sqrt(2)) - ofRational(1, 1, Sqrt(2)))->toEqual(Rational(2, 1, Sqrt(2)))

  expect(ofRational(5, 4, Pi(1)) - ofRational(1, 4, Pi(1)))->toEqual(Rational(1, 1, Pi(1)))

  expect(ofRational(1, 1, Pi(1)) - ofRational(1, 1, Pi(2)))->toEqual(
    ofDecimalString("-6.728011747499565380371847616596649"),
  )
  expect(ofRational(1, 1, Sqrt(2)) - ofRational(1, 1, Sqrt(3)))->toEqual(
    ofDecimalString("-0.317837245195782244725757617296174"),
  )
})

test("sub constants of differing type", (. ()) => {
  expect(ofRational(1, 1, Pi(1)) - ofRational(1, 1, Unit))->toEqual(
    ofDecimalString("2.141592653589793238462643383279503"),
  )
})

test("sub with integer overflows", (. ()) => {
  expect(ofRational(lsl(1, 31), 1, Unit) - ofRational(lsl(1, 30), 1, Unit))->toEqual(
    ofDecimalString("-3221225472"),
  )
  expect(ofRational(lsl(1, 31), 1, Pi(1)) - ofRational(lsl(1, 30), 1, Pi(1)))->toEqual(
    ofDecimalString("-10119778278.39151421894923698667219"),
  )
})

test("mul", (. ()) => {
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

test("mul with constants based on exponents", (. ()) => {
  expect(ofRational(1, 1, Pi(1)) * ofRational(2, 1, Pi(1)))->toEqual(Rational(2, 1, Pi(2)))
  expect(ofRational(1, 1, Exp(1)) * ofRational(2, 1, Exp(1)))->toEqual(Rational(2, 1, Exp(2)))
})

test("mul with differing constants", (. ()) => {
  expect(ofRational(1, 1, Pi(1)) * ofRational(1, 1, Exp(1)))->toEqual(
    ofDecimalString("8.539734222673567065463550869546573"),
  )
  expect(ofRational(1, 1, Pi(1)) * ofRational(1, 1, Sqrt(2)))->toEqual(
    ofDecimalString("4.442882938158366247015880990060694"),
  )
  expect(ofRational(1, 1, Exp(1)) * ofRational(1, 1, Pi(1)))->toEqual(
    ofDecimalString("8.539734222673567065463550869546573"),
  )
  expect(ofRational(1, 1, Exp(1)) * ofRational(1, 1, Sqrt(2)))->toEqual(
    ofDecimalString("3.844231028159116824863671637426276"),
  )
  expect(ofRational(1, 1, Sqrt(2)) * ofRational(1, 1, Pi(1)))->toEqual(
    ofDecimalString("4.442882938158366247015880990060694"),
  )
  expect(ofRational(1, 1, Sqrt(2)) * ofRational(1, 1, Exp(1)))->toEqual(
    ofDecimalString("3.844231028159116824863671637426276"),
  )
})

test("mul with constant-based integer overflows", (. ()) => {
  expect(ofRational(1, 1, Pi(lsl(1, 30))) * ofRational(1, 1, Pi(lsl(1, 30))))->toEqual(
    ofDecimalString("1.644122443189402456517264494595729e+1067621222"),
  )
  expect(ofRational(1, 1, Exp(lsl(1, 30))) * ofRational(1, 1, Exp(lsl(1, 30))))->toEqual(
    ofDecimalString("2.013099393068963956343718245436346e+932640298"),
  )
  expect(ofRational(1, 1, Sqrt(lsl(1, 30))) * ofRational(1, 1, Sqrt(lsl(1, 30))))->toEqual(
    Rational(1073741824, 1, Unit),
  )
})

test("mul with differing constants", (. ()) => {
  expect(ofRational(1, 1, Pi(1)) * ofRational(2, 1, Sqrt(2)))->toEqual(
    ofDecimalString("8.885765876316732494031761980121387"),
  )
  expect(ofRational(1, 1, Sqrt(2)) * ofRational(2, 1, Pi(1)))->toEqual(
    ofDecimalString("8.885765876316732494031761980121387"),
  )
  expect(ofRational(1, 1, Exp(1)) * ofRational(2, 1, Sqrt(2)))->toEqual(
    ofDecimalString("7.688462056318233649727343274852552"),
  )
  expect(ofRational(2, 1, Sqrt(2)) * ofRational(1, 1, Exp(1)))->toEqual(
    ofDecimalString("7.688462056318233649727343274852552"),
  )
})

test("mul with constants based on square roots", (. ()) => {
  expect(ofRational(1, 1, Sqrt(2)) * ofRational(2, 1, Sqrt(3)))->toEqual(Rational(2, 1, Sqrt(6)))
})

test("div", (. ()) => {
  expect(ofRational(1, 3, Unit) / ofRational(2, 3, Unit))->toEqual(Rational(1, 2, Unit))
  expect(ofRational(1, 1, Unit) / ofRational(0, 0, Unit))->toEqual(Rational(1, 0, Unit))

  expect(ofRational(1, 1, Unit) / ofDecimalString("1"))->toEqual(ofDecimalString("1"))
  expect(ofDecimalString("1") / ofRational(1, 1, Unit))->toEqual(ofDecimalString("1"))

  expect(ofRational(1, 1, Unit) / ofRational(1, 1, Pi(1)))->toEqual(Rational(1, 1, Pi(-1)))

  expect(ofRational(1, 1, Pi(1)) / ofDecimalString("1"))->toEqual(
    ofDecimalString("3.141592653589793238462643383279503"),
  )
})

test("div with constants on lhs", (. ()) => {
  expect(ofRational(1, 3, Pi(1)) / ofRational(2, 3, Unit))->toEqual(Rational(1, 2, Pi(1)))
  expect(ofRational(1, 3, Exp(1)) / ofRational(2, 3, Unit))->toEqual(Rational(1, 2, Exp(1)))
  expect(ofRational(1, 3, Sqrt(2)) / ofRational(2, 3, Unit))->toEqual(Rational(1, 2, Sqrt(2)))
})

test("div with constants on rhs", (. ()) => {
  expect(ofRational(2, 1, Unit) / ofRational(1, 1, Sqrt(2)))->toEqual(Rational(1, 1, Sqrt(2)))
})

test("div with matching constants", (. ()) => {
  expect(ofRational(1, 3, Pi(1)) / ofRational(2, 3, Pi(1)))->toEqual(Rational(1, 2, Unit))
  expect(ofRational(1, 3, Exp(1)) / ofRational(2, 3, Exp(1)))->toEqual(Rational(1, 2, Unit))
  expect(ofRational(1, 3, Sqrt(2)) / ofRational(2, 3, Sqrt(2)))->toEqual(Rational(1, 2, Unit))
})

test("div with constants based on exponents", (. ()) => {
  expect(ofRational(1, 3, Pi(2)) / ofRational(2, 3, Pi(1)))->toEqual(Rational(1, 2, Pi(1)))
  expect(ofRational(1, 3, Exp(2)) / ofRational(2, 3, Exp(1)))->toEqual(Rational(1, 2, Exp(1)))
  expect(ofRational(1, 3, Unit) / ofRational(2, 3, Pi(1)))->toEqual(Rational(1, 2, Pi(-1)))
  expect(ofRational(1, 3, Unit) / ofRational(2, 3, Exp(1)))->toEqual(Rational(1, 2, Exp(-1)))

  expect(ofRational(1, 2, Exp(1)) / ofRational(1, 1, Pi(1)))->toEqual(
    ofDecimalString("0.4326279897161325436088873948230447"),
  )
})

test("div with constant-based integer overflows", (. ()) => {
  expect(ofRational(1, 1, Pi(lsl(1, 31))) / ofRational(1, 1, Pi(lsl(1, 30))))->toEqual(
    ofDecimalString("4.74349845415943269813061650923671e-1601431834"),
  )
  expect(ofRational(1, 1, Exp(lsl(1, 31))) / ofRational(1, 1, Exp(lsl(1, 30))))->toEqual(
    ofDecimalString("3.50108111726730731270975056926549e-1398960448"),
  )
})

test("div with sqrt", (. ()) => {
  expect(ofRational(1, 1, Sqrt(6)) / ofRational(1, 1, Sqrt(3)))->toEqual(Rational(1, 1, Sqrt(2)))
  expect(ofRational(1, 1, Sqrt(2)) / ofRational(1, 1, Sqrt(0)))->toEqual(Rational(1, 0, Unit))
})

test("div with differing constants", (. ()) => {
  expect(ofRational(1, 1, Pi(1)) / ofRational(1, 1, Exp(1)))->toEqual(
    ofDecimalString("1.155727349790921717910093183312696"),
  )
  expect(ofRational(1, 1, Pi(1)) / ofRational(1, 1, Sqrt(2)))->toEqual(
    ofDecimalString("2.221441469079183123507940495030347"),
  )
  expect(ofRational(1, 1, Exp(1)) / ofRational(1, 1, Pi(1)))->toEqual(
    ofDecimalString("0.8652559794322650872177747896460894"),
  )
  expect(ofRational(1, 1, Exp(1)) / ofRational(1, 1, Sqrt(2)))->toEqual(
    ofDecimalString("1.922115514079558412431835818713138"),
  )
  expect(ofRational(1, 1, Sqrt(2)) / ofRational(1, 1, Pi(1)))->toEqual(
    ofDecimalString("0.4501581580785530347775995955033702"),
  )
  expect(ofRational(1, 1, Sqrt(2)) / ofRational(1, 1, Exp(1)))->toEqual(
    ofDecimalString("0.5202600950228888963581524433163326"),
  )
})

test("powInt", (. ()) => {
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

test("powInt with constants", (. ()) => {
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

test("powInt with integer overflows", (. ()) => {
  expect(powInt(ofRational(lsl(1, 30), 1, Unit), 2))->toEqual(
    ofDecimalString("1152921504606846976"),
  )
  expect(powInt(ofRational(lsl(1, 30), 1, Unit), 3))->toEqual(
    ofDecimalString("1.237940039285380274899124224e+27"),
  )

  expect(powInt(ofRational(lsl(1, 30), 1, Unit), -2))->toEqual(
    ofDecimalString("8.673617379884035472059622406959534e-19"),
  )
  expect(powInt(ofRational(lsl(1, 30), 1, Unit), -3))->toEqual(
    ofDecimalString("8.077935669463160887416100508495731e-28"),
  )
})

test("powInt with constant-based integer overflows", (. ()) => {
  let largeNumberNotSimplifiable = {
    open Belt.Int
    lsl(1, 30) - 5
  }

  expect(powInt(ofRational(1, 1, Pi(lsl(1, 31))), -1))->toEqual(
    ofDecimalString("1.644122443189402456517264494595728e+1067621222"),
  )
  expect(powInt(ofRational(1, 1, Exp(lsl(1, 31))), -1))->toEqual(
    ofDecimalString("2.013099393068963956343718245436345e+932640298"),
  )
  expect(powInt(ofRational(lsl(1, 30), 1, Sqrt(largeNumberNotSimplifiable)), -1))->toEqual(
    ofDecimalString("2.842170949657845667019877891613702e-14"),
  )

  expect(powInt(ofRational(1, 1, Pi(lsl(1, 31))), -3))->toEqual(
    ofDecimalString("4.444290852791888622739833035408304e+3202863666"),
  )
  expect(powInt(ofRational(1, 1, Exp(lsl(1, 31))), -3))->toEqual(
    ofDecimalString("8.158224529198766973002604485250485e+2797920894"),
  )
  expect(powInt(ofRational(lsl(1, 30), 1, Sqrt(largeNumberNotSimplifiable)), -3))->toEqual(
    ofDecimalString("2.295887419986368642899842101140575e-41"),
  )
})

test("pow with precision loss", (. ()) => {
  expect(pow(ofInt(64), ofRational(1, 3, Unit)))->toEqual(Rational(4, 1, Unit))
})
