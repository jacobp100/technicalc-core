open UrlSafeEncoding

%%private(let boldFlag = 0b01)
%%private(let italicFlag = 0b01)

let encodeSymbol = (input: Symbol.t) => {
  let styleUint = (input.bold ? boldFlag : 0)->lor(input.italic ? italicFlag : 0)
  encodeUint(styleUint) ++
  encodeString(input.base) ++
  encodeString(input.superscript) ++
  encodeString(input.subscript)
}

let readSymbol = (reader): option<Symbol.t> =>
  switch (readUint(reader), readString(reader), readString(reader), readString(reader)) {
  | (Some(styleUint), Some(base), Some(superscript), Some(subscript)) =>
    let bold = land(styleUint, boldFlag) != 0
    let italic = land(styleUint, italicFlag) != 0
    Some({bold, italic, base, superscript, subscript})
  | _ => None
  }
