type t

external ofInt: int => t = "%identity"
@module("./SafeInt.js") external toInt: t => option<int> = "toInt"
@module("./SafeInt.js") external abs: t => t = "abs"
@module("./SafeInt.js") external neg: t => t = "neg"
@module("./SafeInt.js") external add: (t, t) => t = "add"
@module("./SafeInt.js") external sub: (t, t) => t = "sub"
@module("./SafeInt.js") external mul: (t, t) => t = "mul"
@module("./SafeInt.js") external div: (t, t) => t = "div"
@module("./SafeInt.js") external pow: (t, t) => t = "pow"
@module("./SafeInt.js") external rem: (t, t) => t = "mod"
@module("./SafeInt.js") external \"~-": t => t = "neg"
@module("./SafeInt.js") external \"+": (t, t) => t = "add"
@module("./SafeInt.js") external \"-": (t, t) => t = "sub"
@module("./SafeInt.js") external \"*": (t, t) => t = "mul"
@module("./SafeInt.js") external \"/": (t, t) => t = "div"
@module("./SafeInt.js") external \"**": (t, t) => t = "pow"
@module("./SafeInt.js") external mod: (t, t) => t = "mod"

let negInt = a => ofInt(a)->neg->toInt
let absInt = a => ofInt(a)->abs->toInt
let addInt = (a, b) => (ofInt(a) + ofInt(b))->toInt
let subInt = (a, b) => (ofInt(a) - ofInt(b))->toInt
let mulInt = (a, b) => (ofInt(a) * ofInt(b))->toInt
let divInt = (a, b) => (ofInt(a) / ofInt(b))->toInt
let modInt = (a, b) => mod(ofInt(a), ofInt(b))->toInt
