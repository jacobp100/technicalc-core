open Jest
open Encoding

test("unsigned integers", (. ()) => {
  for value in 0 to 4096 {
    let encoded = encodeUint(value)

    let decoded = read(encoded, readUint)
    expect(decoded)->toEqual(Some(value))
  }

  // This should work - it's just a very inefficient encoding
  expect(encodeUint(-1)->read(readUint))->toEqual(Some(-1))
})

test("integers", (. ()) => {
  expect(encodeInt(1)->read(readInt))->toEqual(Some(1))
  expect(encodeInt(-1)->read(readInt))->toEqual(Some(-1))
  expect(encodeInt(5)->read(readInt))->toEqual(Some(5))
  expect(encodeInt(-5)->read(readInt))->toEqual(Some(-5))

  expect(encodeInt(1074000000)->read(readInt))->toEqual(Some(1074000000))
})

test("strings", (. ()) => {
  let string = "Hello world !@#$%^&*()"
  let encoded = encodeString(~optimizeFor=Text, string)
  let decoded = read(encoded, readString(~optimizeFor=Text))
  expect(decoded)->toEqual(Some(string))
})

test("string optimisation for text", (. ()) => {
  let string = "helloworld"
  let encoded = encodeString(~optimizeFor=Text, string)
  let decoded = read(encoded, readString(~optimizeFor=Text))
  expect(decoded)->toEqual(Some(string))
  expect(String.length(encoded))->toEqual(String.length(string) + 1)
})

test("string optimisation for numbers", (. ()) => {
  let string = "+-1234567890"
  let encoded = encodeString(~optimizeFor=Numbers, string)
  let decoded = read(encoded, readString(~optimizeFor=Numbers))
  expect(decoded)->toEqual(Some(string))
  expect(String.length(encoded))->toEqual(String.length(string) + 1)
})

test("arrays", (. ()) => {
  let array = [1, 2, 3]
  let encoded = encodeArray(array, encodeUint)
  let decoded = read(encoded, reader => readArray(reader, readUint))
  expect(decoded)->toEqual(Some(array))
})
