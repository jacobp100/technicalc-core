open Jest
open Real

test("rem with integers", (. ()) => {
  expect(rem(ofRational(9, 1, Unit), ofRational(4, 1, Unit)))->toEqual(Rational(1, 1, Unit))
  expect(rem(ofRational(-9, 1, Unit), ofRational(4, 1, Unit)))->toEqual(Rational(3, 1, Unit))
  expect(rem(ofRational(9, 1, Unit), ofRational(-4, 1, Unit)))->toEqual(Rational(-3, 1, Unit))
  expect(rem(ofRational(-9, 1, Unit), ofRational(-4, 1, Unit)))->toEqual(Rational(-1, 1, Unit))
})

test("rem with fractions", (. ()) => {
  expect(rem(ofRational(3, 2, Unit), ofRational(1, 2, Unit)))->toEqual(Rational(0, 1, Unit))
  expect(rem(ofRational(5, 2, Unit), ofRational(1, 1, Unit)))->toEqual(Rational(1, 2, Unit))
  expect(rem(ofRational(-5, 2, Unit), ofRational(1, 1, Unit)))->toEqual(Rational(1, 2, Unit))
  expect(rem(ofRational(5, 2, Unit), ofRational(-1, 1, Unit)))->toEqual(Rational(-1, 2, Unit))
  expect(rem(ofRational(-5, 2, Unit), ofRational(-1, 1, Unit)))->toEqual(Rational(-1, 2, Unit))
})
