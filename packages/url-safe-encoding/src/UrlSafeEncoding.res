type reader = {
  string: string,
  mutable index: option<int>,
}

%%private(
  let readInvalid = reader => {
    reader.index = None
    None
  }
)

let read = (string, decoder) => {
  let reader = {string: string, index: Some(0)}
  let value = decoder(reader)

  let isComplete = switch reader.index {
  | Some(index) => index === String.length(string)
  | None => false
  }

  isComplete ? value : None
}

/* Character set from URL-safe base64 variant */
%%private(let characters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_")

%%private(
  @inline
  let charToIndex = (character: int) => {
    let character: char = Obj.magic(character)
    switch character {
    | 'a' .. 'z' => Char.code(character) - Char.code('a')
    | 'A' .. 'Z' => Char.code(character) - Char.code('A') + 26
    | '0' .. '9' => Char.code(character) - Char.code('0') + 52
    | '-' => 62
    | '_' => 63
    | _ => assert false
    }
  }
)

%%private(let uintBase = 5)
%%private(let uintBit = lsl(1, uintBase))
%%private(let uintMask = uintBit - 1)

let encodeUint = value => {
  // Handles negative numbers too - it's just not optimised for them
  let rec iter = (~current="", ~flag=false, value) => {
    let power = lsr(value, uintBase)
    let char = StringUtil.stringCharAtUnsafe(
      characters,
      lor(land(value, uintMask), flag ? uintBit : 0),
    )
    let current = char ++ current
    if power == 0 {
      current
    } else {
      iter(~current, ~flag=true, power)
    }
  }
  iter(value)
}

let readUint = ({string, index} as reader): option<int> => {
  let rec iter = (~accum=0, index) =>
    if index < String.length(string) {
      let value = StringUtil.charAtUnsafe(string, index)->charToIndex

      if land(value, lnot(uintMask)) != 0 {
        iter(~accum=lsl(lor(accum, land(value, uintMask)), uintBase), index + 1)
      } else {
        reader.index = Some(index + 1)
        Some(lor(accum, value))
      }
    } else {
      readInvalid(reader)
    }

  switch index {
  | Some(index) => iter(index)
  | None => None
  }
}

let encodeInt = value => {
  let value = value < 0 ? lnot(value)->lsl(1)->lor(1) : lsl(value, 1)
  encodeUint(value)
}

let readInt = reader =>
  switch readUint(reader) {
  | Some(value) => Some(land(value, 1) != 0 ? lsr(value, 1)->lnot : lsr(value, 1))
  | None => None
  }

let encodeArray = (array, encoder) =>
  encodeUint(Belt.Array.length(array)) ++ Belt.Array.map(array, encoder)->StringUtil.join

let readArray = (reader, decoder) =>
  switch readUint(reader) {
  | Some(length) =>
    let out = Belt.Array.makeUninitializedUnsafe(length)

    let rec iter = i =>
      if i < length {
        switch decoder(reader) {
        | Some(value) =>
          Belt.Array.setExn(out, i, value)
          iter(i + 1)
        | None => readInvalid(reader)
        }
      } else {
        Some(out)
      }

    iter(0)
  | _ => readInvalid(reader)
  }

type stringOptimisation =
  | Text
  | Numbers

%%private(
  let charXor = x =>
    switch x {
    | Text => 0b1100100
    | Numbers => 0b0100000
    }
)

let encodeString = (~optimizeFor=Text, string) => {
  let charXor = charXor(optimizeFor)
  Belt.Array.makeByU(String.length(string), (. i) => {
    lxor(StringUtil.charAtUnsafe(string, i), charXor)
  })->encodeArray(encodeUint)
}

let readString = (~optimizeFor=Text, reader) => {
  let charXor = charXor(optimizeFor)
  let readChar = reader =>
    switch readUint(reader) {
    | Some(char) => Some(StringUtil.ofChar(lxor(char, charXor)))
    | None => readInvalid(reader)
    }

  switch readArray(reader, readChar) {
  | Some(charArray) => Some(StringUtil.join(charArray))
  | None => readInvalid(reader)
  }
}
