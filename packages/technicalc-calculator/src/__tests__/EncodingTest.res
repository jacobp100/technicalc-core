open Jest
open Value
open Encoding

test("zero", () => {
  let value = zero
  let result = encode(value)->decode
  expect(result)->toEqual(Some(value))
})

test("real rational", () => {
  let value = ofInt(1)
  let result = encode(value)->decode
  expect(result)->toEqual(Some(value))
})

test("real with constants", () => {
  let valueOfConstant = c => Value.ofReal(Real.ofRational(1, 1, c))

  let pi = valueOfConstant(Pi(1))
  expect(encode(pi)->decode)->toEqual(Some(pi))

  let e = valueOfConstant(Exp(1))
  expect(encode(e)->decode)->toEqual(Some(e))

  let sqrt2 = valueOfConstant(Sqrt(2))
  expect(encode(sqrt2)->decode)->toEqual(Some(sqrt2))
})

test("negative rational", () => {
  let value = ofInt(-1)
  let result = encode(value)->decode
  expect(result)->toEqual(Some(value))
})

test("real decimal", () => {
  // Any value that cannot be encoded as a rational
  let value = {
    open Decimal
    one / pi
  }->ofDecimal
  let result = encode(value)->decode
  expect(result)->toEqual(Some(value))
})

test("imag rational", () => {
  let value = ofInt(1)->mul(i)
  let result = encode(value)->decode
  expect(result)->toEqual(Some(value))
})

test("imag decimal", () => {
  // Any value that cannot be encoded as a rational
  let value =
    {
      open Decimal
      one / pi
    }
    ->ofDecimal
    ->mul(i)
  let result = encode(value)->decode
  expect(result)->toEqual(Some(value))
})

test("complex rational", () => {
  let value = ofInt(1)
  let value = add(value, mul(value, i))
  let result = encode(value)->decode
  expect(result)->toEqual(Some(value))
})

test("complex decimal", () => {
  // Any value that cannot be encoded as a rational
  let value = {
    open Decimal
    one / pi
  }->ofDecimal
  let value = add(value, mul(value, i))
  let result = encode(value)->decode
  expect(result)->toEqual(Some(value))
})

test("vector", () => {
  let value = Vector.make([Scalar.ofFloat(1.), Scalar.ofFloat(2.), Scalar.ofFloat(3.)])->ofVector
  let result = encode(value)->decode
  expect(result)->toEqual(Some(value))
})

test("matrix", () => {
  let elements = [Scalar.ofFloat(1.), Scalar.ofFloat(2.), Scalar.ofFloat(3.), Scalar.ofFloat(4.)]
  let value = Matrix.make(~numRows=2, ~numColumns=2, elements)->ofMatrix
  let result = encode(value)->decode
  expect(result)->toEqual(Some(value))
})

test("percent", () => {
  let value = ofPercent(Scalar.one)
  let result = encode(value)->decode
  expect(result)->toEqual(Some(value))
})

test("nan", () => {
  let value = nan
  let result = encode(value)->decode
  expect(result)->toEqual(Some(value))
})
