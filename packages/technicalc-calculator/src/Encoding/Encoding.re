/*
 URL-safe encoding to string

 Also used directly in technicalc-editor
 */

type reader = {
  string,
  mutable index: option(int),
};

let%private readInvalid = reader => {
  reader.index = None;
  None;
};

let read = (string, decoder) => {
  let reader = {string, index: Some(0)};
  let value = decoder(reader);

  let isComplete =
    switch (reader.index) {
    | Some(index) => index === String.length(string)
    | None => false
    };

  isComplete ? value : None;
};

/* Character set from URL-safe base64 variant */
let%private characters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_";

let%private charToIndex = (character: int) => {
  let character: char = Obj.magic(character);
  switch (character) {
  | 'a'..'z' => Char.code(character) - Char.code('a')
  | 'A'..'Z' => Char.code(character) - Char.code('A') + 26
  | '0'..'9' => Char.code(character) - Char.code('0') + 52
  | '-' => 62
  | '_' => 63
  | _ => assert(false)
  };
};

let encodeUint = value => {
  assert(value >= 0);

  let rec iter = (~current="", ~flag=false, value) => {
    let power = value / 32;
    let char =
      StringUtil.stringCharAtUnsafe(
        characters,
        value mod 32 + (flag ? 32 : 0),
      );
    let current = char ++ current;
    if (power == 0) {
      current;
    } else {
      iter(~current, ~flag=true, power);
    };
  };
  iter(value);
};

let readUint = ({string, index} as reader): option(int) => {
  let rec iter = (~accum=0, index) =>
    if (index < String.length(string)) {
      let value = StringUtil.charAtUnsafe(string, index)->charToIndex;

      if (value >= 32) {
        iter(~accum=(accum + value mod 32) * 32, index + 1);
      } else {
        reader.index = Some(index + 1);
        Some(accum + value);
      };
    } else {
      readInvalid(reader);
    };

  switch (index) {
  | Some(index) => iter(index)
  | None => None
  };
};

let encodeInt = value => {
  let value = abs(value) lsl 1 lor (value < 0 ? 1 : 0);
  encodeUint(value);
};

let readInt = reader =>
  switch (readUint(reader)) {
  | Some(value) => Some(value lsr 1 * (value land 1 !== 0 ? (-1) : 1))
  | None => None
  };

let encodeArray = (array, encoder) =>
  encodeUint(Belt.Array.length(array))
  ++ Belt.Array.mapU(array, encoder)->StringUtil.join;

let readArray = (reader, decoder) =>
  switch (readUint(reader)) {
  | Some(length) =>
    let out = Belt.Array.makeUninitializedUnsafe(length);

    let rec iter = i =>
      if (i < length) {
        switch (decoder(. reader)) {
        | Some(value) =>
          Belt.Array.setExn(out, i, value);
          iter(i + 1);
        | None => readInvalid(reader)
        };
      } else {
        Some(out);
      };

    iter(0);
  | _ => readInvalid(reader)
  };

type stringOptimisation =
  | Text
  | Numbers;

let%private charXor = x =>
  switch (x) {
  | Text =>
    // Values 0-31: "defg`abclmnohijktuvwpqrs|}~xyz"
    // The 0b100 at the bottom slightly scrambles the text
    // This avoids having readable text showing up in the output
    0b1100100
  | Numbers =>
    // Values 0-31: " !\"#$%&'()*+,-./0123456789:;<=>"
    0b0100000
  };

let encodeString = (~optimizeFor=Text, string) => {
  let charXor = charXor(optimizeFor);
  Belt.Array.makeByU(String.length(string), (. i) => {
    StringUtil.charAtUnsafe(string, i) lxor charXor
  })
  ->encodeArray((. value) => encodeUint(value));
};

let readString = (~optimizeFor=Text, reader) => {
  let charXor = charXor(optimizeFor);
  let readChar =
    (. reader) =>
      switch (readUint(reader)) {
      | Some(char) => Some(StringUtil.ofChar(char lxor charXor))
      | None => readInvalid(reader)
      };

  switch (readArray(reader, readChar)) {
  | Some(charArray) => Some(StringUtil.join(charArray))
  | None => readInvalid(reader)
  };
};
