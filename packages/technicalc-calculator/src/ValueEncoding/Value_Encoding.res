let encode = Encoding_Value.encodeValue

let decode = encoded => Encoding.read(encoded, Encoding_Value.readValue)
