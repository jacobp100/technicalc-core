open Jest;
open Value;

test("real rational", (.) => {
  let value = ofInt(1);
  let result = encode(value)->decode;
  expect(result)->toEqual(Some(value));
});

test("real decimal", (.) => {
  // Any value that cannot be encoded as a rational
  let value = Decimal.(one / pi)->ofDecimal;
  let result = encode(value)->decode;
  expect(result)->toEqual(Some(value));
});

test("imag rational", (.) => {
  let value = ofInt(1)->mul(i);
  let result = encode(value)->decode;
  expect(result)->toEqual(Some(value));
});

test("imag decimal", (.) => {
  // Any value that cannot be encoded as a rational
  let value = Decimal.(one / pi)->ofDecimal->mul(i);
  let result = encode(value)->decode;
  expect(result)->toEqual(Some(value));
});

test("complex rational", (.) => {
  let value = ofInt(1);
  let value = add(value, mul(value, i));
  let result = encode(value)->decode;
  expect(result)->toEqual(Some(value));
});

test("complex decimal", (.) => {
  // Any value that cannot be encoded as a rational
  let value = Decimal.(one / pi)->ofDecimal;
  let value = add(value, mul(value, i));
  let result = encode(value)->decode;
  expect(result)->toEqual(Some(value));
});

test("vector", (.) => {
  let value =
    ofVector([|
      Scalar.ofFloat(1.),
      Scalar.ofFloat(2.),
      Scalar.ofFloat(3.),
    |]);
  let result = encode(value)->decode;
  expect(result)->toEqual(Some(value));
});

test("matrix", (.) => {
  let elements = [|
    Scalar.ofFloat(1.),
    Scalar.ofFloat(2.),
    Scalar.ofFloat(3.),
    Scalar.ofFloat(4.),
  |];
  let value = Matrix.make(2, 2, elements)->ofMatrix;
  let result = encode(value)->decode;
  expect(result)->toEqual(Some(value));
});

test("percent", (.) => {
  let value = ofPercent(Scalar.one);
  let result = encode(value)->decode;
  expect(result)->toEqual(Some(value));
});

test("nan", (.) => {
  let value = nan;
  let result = encode(value)->decode;
  expect(result)->toEqual(Some(value));
});
