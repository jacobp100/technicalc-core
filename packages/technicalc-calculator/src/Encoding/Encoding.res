open UrlSafeEncoding

let encode = Encoding_Value.encode

let decode = encoded => read(encoded, Encoding_Value.read)
