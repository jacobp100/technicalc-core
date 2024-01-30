type t
type constructor

@val @module("decimal.js")
external constructor: constructor = "default"
@send external set: (constructor, 'a) => unit = "set"

@new @module("decimal.js") external ofInt: int => t = "default"
@new @module("decimal.js") external ofFloat: float => t = "default"
@new @module("decimal.js") external ofString: string => t = "default"
@send external toFloat: t => float = "toNumber"
@send external toBinary: t => string = "toBinary"
@send external toOctal: t => string = "toOctal"
@send external toHexadecimal: t => string = "toHexadecimal"
@send external toString: t => string = "toFixed"
let zero = ofInt(0)
let one = ofInt(1)
let minusOne = ofInt(-1)
let nan = ofFloat(nan)
@module("decimal.js") @scope("default")
external pi: @as(-1) _ => t = "acos"
@send external isFinite: t => bool = "isFinite"
@send external cmp: (t, t) => int = "cmp"
@send external eq: (t, t) => bool = "eq"
@send external gt: (t, t) => bool = "gt"
@send external gte: (t, t) => bool = "gte"
@send external lt: (t, t) => bool = "lt"
@send external lte: (t, t) => bool = "lte"
@module("decimal.js") @scope("default")
external inv: (@as(1) _, t) => t = "div"
@send external neg: t => t = "neg"
@send external abs: t => t = "abs"
@send external acos: t => t = "acos"
@send external acosh: t => t = "acosh"
@send external add: (t, t) => t = "add"
@send external asin: t => t = "asin"
@send external asinh: t => t = "asinh"
@send external atan: t => t = "atan"
@send external atanh: t => t = "atanh"
@send external cbrt: t => t = "cbrt"
@send external ceil: t => t = "ceil"
@send external cos: t => t = "cos"
@send external cosh: t => t = "cosh"
@send external div: (t, t) => t = "div"
@send external exp: t => t = "exp"
@send external floor: t => t = "floor"
@send external ln: t => t = "ln"
@send external logBase: (t, t) => t = "log"
@send external log: t => t = "log"
@send external rem: (t, t) => t = "mod"
@send external mul: (t, t) => t = "mul"
@send external pow: (t, t) => t = "pow"
@send external random: unit => t = "random"
@send external round: t => t = "round"
@send external sign: t => t = "sign"
@send external sin: t => t = "sin"
@send external sinh: t => t = "sinh"
@send external sqrt: t => t = "sqrt"
@send external sub: (t, t) => t = "sub"
@send external tan: t => t = "tan"
@send external tanh: t => t = "tanh"
@send external trunc: t => t = "trunc"
@module("decimal.js") @scope("default")
external atan2: (t, t) => t = "atan2"
@module("decimal.js") @scope("default") external log2: t => t = "log2"
@module("decimal.js") @scope("default")
external log10: t => t = "log10"
// [@module "decimal.js"] [@scope "default"] [@variadic]
// external hypot: array(t) => t = "hypot";
// [@module "decimal.js"] [@scope "default"] [@variadic]
// external max: array(t) => t = "max";
// [@module "decimal.js"] [@scope "default"] [@variadic]
// external min: array(t) => t = "min";

// When using @jacobp100/react-native-webworker, it's possible to terminate
// work in the middle of the function. All this code is pure, so that's not an
// issue. However, decimal.js has some stateful flags that get set -
// `external`, `inexact`, and `quadrant`. The latter two are only used to
// simulate multiple return values - so should always be set before they're
// used. The last is set and unset during some computations. This does need
// resetting in case it gets set, then the execution terminated before it
// unsets. Fortunately, the `hypot` function will reset the value with almost
// zero overhead.
@module("decimal.js") @scope("default")
external resetInternalState: unit => unit = "hypot"

@send external \"=": (t, t) => bool = "eq"
let \"<>" = (a, b) => !(a == b)
@send external \">": (t, t) => bool = "gt"
@send external \">=": (t, t) => bool = "gte"
@send external \"<": (t, t) => bool = "lt"
@send external \"<=": (t, t) => bool = "lte"
@send external \"~-": t => t = "neg"
@send external \"+": (t, t) => t = "add"
@send external \"-": (t, t) => t = "sub"
@send external \"*": (t, t) => t = "mul"
@send external \"/": (t, t) => t = "div"
@send external \"**": (t, t) => t = "pow"
@send external mod: (t, t) => t = "mod"
