open Jest
open UrlSafeEncoding

test("unsigned integers", () => {
  for value in 0 to 4096 {
    let encoded = encodeUint(value)

    let decoded = read(encoded, readUint)
    expect(decoded)->toEqual(Some(value))
  }

  // This should work - it's just a very inefficient encoding
  expect(encodeUint(-1)->read(readUint))->toEqual(Some(-1))
})

test("integers", () => {
  for value in 0 to 4096 {
    expect(encodeInt(value)->read(readInt))->toEqual(Some(value))
    expect(encodeInt(-value)->read(readInt))->toEqual(Some(-value))
  }

  expect(encodeInt(1074000000)->read(readInt))->toEqual(Some(1074000000))
})

test("strings", () => {
  let string = "Hello world !@#$%^&*()"
  let encoded = encodeString(~optimizeFor=Text, string)
  let decoded = read(encoded, readString(~optimizeFor=Text, _))
  expect(decoded)->toEqual(Some(string))
})

test("string optimisation for text", () => {
  let string = "helloworld"
  let encoded = encodeString(~optimizeFor=Text, string)
  let decoded = read(encoded, readString(~optimizeFor=Text, _))
  expect(decoded)->toEqual(Some(string))
  expect(String.length(encoded))->toEqual(String.length(string) + 1)
})

test("string optimisation for numbers", () => {
  let string = "+-1234567890"
  let encoded = encodeString(~optimizeFor=Numbers, string)
  let decoded = read(encoded, readString(~optimizeFor=Numbers, _))
  expect(decoded)->toEqual(Some(string))
  expect(String.length(encoded))->toEqual(String.length(string) + 1)
})

test("arrays", () => {
  let array = [1, 2, 3]
  let encoded = encodeArray(array, encodeUint)
  let decoded = read(encoded, reader => readArray(reader, readUint))
  expect(decoded)->toEqual(Some(array))
})

test("invalid encodings", () => {
  expect(read("ueaueoueoa", readUint))->toBe(None)
  expect(read("ueaueoueoa", readInt))->toBe(None)
  expect(read("ueaueoueoa", readString(~optimizeFor=Text, _)))->toBe(None)
  expect(read("ueaueoueoa", readString(~optimizeFor=Numbers, _)))->toBe(None)
  expect(read("ueaueoueoa", readArray(_, readUint)))->toBe(None)
  expect(read("ueaueoueoa", readArray(_, readString(~optimizeFor=Text, _))))->toBe(None)

  expect(read("", readUint))->toBe(None)
  expect(read("", readInt))->toBe(None)
  expect(read("", readString(~optimizeFor=Text, _)))->toBe(None)
  expect(read("", readString(~optimizeFor=Numbers, _)))->toBe(None)
  expect(read("", readArray(_, readUint)))->toBe(None)
  expect(read("", readArray(_, readString(~optimizeFor=Text, _))))->toBe(None)

  // This is just to get 100% code coverage for this file
  expect(() => ignore(read("@", readUint)))->toThrow()
  expect(() => ignore(read("!", readUint)))->toThrow()
  expect(() => ignore(read("~", readUint)))->toThrow()
  expect(() => ignore(read("[", readUint)))->toThrow()
})

test("multiple reads on invalidated reader", () => {
  let _ = read("ueoaueo", reader => {
    let pass = switch (readString(reader), readUint(reader)) {
    | (None, None) => true
    | _ => false
    }

    expect(pass)->toBe(true)

    None
  })
})
