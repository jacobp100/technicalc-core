open UrlSafeEncoding

let encode = Encoding_Value.encode

let decode = encoded => read(encoded, Encoding_Value.read)

let encodeUnits = Encoding_Units.encodeUnits

let decodeUnits = encoded => read(encoded, Encoding_Units.readUnits)
