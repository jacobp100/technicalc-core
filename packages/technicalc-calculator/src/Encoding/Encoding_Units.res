open UrlSafeEncoding
open Encoding_Unit

let encodeUnits = units => encodeArray(Units_Base.toArray(units), encodeUnit)

let readUnits = reader =>
  switch readArray(reader, readUnit) {
  | Some(units) => Some(Units_Base.ofArray(units))
  | None => None
  }
