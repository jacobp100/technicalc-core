type t
@new @module("../../decimal/decimal")
external ofInt: int => t = "default"
@new @module("../../decimal/decimal")
external ofFloat: float => t = "default"
@new @module("../../decimal/decimal")
external ofString: string => t = "default"
@send external toFloat: t => float = "toNumber"
@send external toBinary: t => string = "toBinary"
@send external toOctal: t => string = "toOctal"
@send external toHexadecimal: t => string = "toHexadecimal"
@send external toString: t => string = "toString"
let zero = ofInt(0)
let one = ofInt(1)
let minusOne = ofInt(-1)
let nan = ofFloat(nan)
@module("../../decimal/decimal")
external pi: @as(-1) _ => t = "acos"
@module("../../decimal/decimal") external isFinite: t => bool = "isFinite"
@module("../../decimal/decimal") external cmp: (t, t) => int = "cmp"
@module("../../decimal/decimal") external eq: (t, t) => bool = "eq"
@module("../../decimal/decimal") external gt: (t, t) => bool = "gt"
@module("../../decimal/decimal") external gte: (t, t) => bool = "gte"
@module("../../decimal/decimal") external lt: (t, t) => bool = "lt"
@module("../../decimal/decimal") external lte: (t, t) => bool = "lte"
@module("../../decimal/decimal")
external inv: (@as(1) _, t) => t = "div"
@module("../../decimal/decimal") external neg: t => t = "neg"
@module("../../decimal/decimal") external abs: t => t = "abs"
@module("../../decimal/decimal") external acos: t => t = "acos"
@module("../../decimal/decimal") external acosh: t => t = "acosh"
@module("../../decimal/decimal") external add: (t, t) => t = "add"
@module("../../decimal/decimal") external asin: t => t = "asin"
@module("../../decimal/decimal") external asinh: t => t = "asinh"
@module("../../decimal/decimal") external atan: t => t = "atan"
@module("../../decimal/decimal") external atanh: t => t = "atanh"
@module("../../decimal/decimal") external cbrt: t => t = "cbrt"
@module("../../decimal/decimal") external ceil: t => t = "ceil"
@module("../../decimal/decimal") external cos: t => t = "cos"
@module("../../decimal/decimal") external cosh: t => t = "cosh"
@module("../../decimal/decimal") external div: (t, t) => t = "div"
@module("../../decimal/decimal") external exp: t => t = "exp"
@module("../../decimal/decimal") external floor: t => t = "floor"
@module("../../decimal/decimal") external ln: t => t = "ln"
@module("../../decimal/decimal") external log: (t, t) => t = "log"
@module("../../decimal/decimal") external rem: (t, t) => t = "mod"
@module("../../decimal/decimal") external mul: (t, t) => t = "mul"
@module("../../decimal/decimal") external pow: (t, t) => t = "pow"
@module("../../decimal/decimal") external random: unit => t = "random"
@module("../../decimal/decimal") external round: t => t = "round"
@module("../../decimal/decimal") external sign: t => t = "sign"
@module("../../decimal/decimal") external sin: t => t = "sin"
@module("../../decimal/decimal") external sinh: t => t = "sinh"
@module("../../decimal/decimal") external sqrt: t => t = "sqrt"
@module("../../decimal/decimal") external sub: (t, t) => t = "sub"
@module("../../decimal/decimal") external tan: t => t = "tan"
@module("../../decimal/decimal") external tanh: t => t = "tanh"
@module("../../decimal/decimal") external trunc: t => t = "trunc"
@module("../../decimal/decimal") external atan2: (t, t) => t = "atan2"
@module("../../decimal/decimal") external log2: t => t = "log2"
@module("../../decimal/decimal") external log10: t => t = "log10"
// [@module "../../decimal/decimal"] [@scope "default"] [@variadic]
// external hypot: array(t) => t = "hypot";
// [@module "../../decimal/decimal"] [@scope "default"] [@variadic]
// external max: array(t) => t = "max";
// [@module "../../decimal/decimal"] [@scope "default"] [@variadic]
// external min: array(t) => t = "min";

@module("../../decimal/decimal") external \"=": (t, t) => bool = "eq"
let \"<>" = (a, b) => !(a == b)
@module("../../decimal/decimal") external \">": (t, t) => bool = "gt"
@module("../../decimal/decimal") external \">=": (t, t) => bool = "gte"
@module("../../decimal/decimal") external \"<": (t, t) => bool = "lt"
@module("../../decimal/decimal") external \"<=": (t, t) => bool = "lte"
@module("../../decimal/decimal") external \"~-": t => t = "neg"
@module("../../decimal/decimal") external \"+": (t, t) => t = "add"
@module("../../decimal/decimal") external \"-": (t, t) => t = "sub"
@module("../../decimal/decimal") external \"*": (t, t) => t = "mul"
@module("../../decimal/decimal") external \"/": (t, t) => t = "div"
@module("../../decimal/decimal") external \"**": (t, t) => t = "pow"
@module("../../decimal/decimal") external mod: (t, t) => t = "mod"
