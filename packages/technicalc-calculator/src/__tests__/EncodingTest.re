open Jest;
open Encoding;

test("unsigned integers", (.) => {
  for (value in 0 to 4096) {
    let encoded = encodeUint(value);

    let decoded = read(encoded, readUint);
    expect(decoded)->toEqual(Some(value));
  }
});

test("integers", (.) => {
  expect(encodeInt(5)->read(readInt))->toEqual(Some(5));
  expect(encodeInt(-5)->read(readInt))->toEqual(Some(-5));
});

test("strings", (.) => {
  let string = "Hello world !@#$%^&*()";
  let encoded = encodeString(string);
  let decoded = read(encoded, readString);
  expect(decoded)->toEqual(Some(string));
});

test("arrays", (.) => {
  let array = [|1, 2, 3|];
  let encoded = encodeArray(array, (. value) => encodeUint(value));
  let decoded =
    read(encoded, reader => {
      readArray(reader, (. reader) => readUint(reader))
    });
  expect(decoded)->toEqual(Some(array));
});
