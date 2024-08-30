open AST_Types

let encode = (ast: t) => Obj.magic(ast)->Js_json.stringify->UrlSafeEncoding.encodeString

let read = (reader): option<t> =>
  switch UrlSafeEncoding.readString(reader) {
  | Some(json) =>
    try {
      Js_json.parseExn(json)->Obj.magic->Some
    } catch {
    | _ => None
    }
  | None => None
  }
