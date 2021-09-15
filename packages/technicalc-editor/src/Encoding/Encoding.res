open UrlSafeEncoding

let encode = input => Encoding_Elements.encodeElements(input)

let decode = string => read(string, Encoding_Elements.readElements)
