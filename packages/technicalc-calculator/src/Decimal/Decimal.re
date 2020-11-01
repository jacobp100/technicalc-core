type t;
[@bs.new] [@bs.module "./Decimal"] external ofInt: int => t = "default";
[@bs.new] [@bs.module "./Decimal"] external ofFloat: float => t = "default";
[@bs.new] [@bs.module "./Decimal"] external ofString: string => t = "default";
[@bs.send] external toFloat: t => float = "toNumber";
[@bs.send] external toBinary: t => string = "toBinary";
[@bs.send] external toOctal: t => string = "toOctal";
[@bs.send] external toHexadecimal: t => string = "toHexadecimal";
[@bs.send] external toString: t => string = "toString";
let zero = ofInt(0);
let one = ofInt(1);
let minusOne = ofInt(-1);
let nan = ofFloat(nan);
[@bs.module "./Decimal"] [@bs.scope "default"]
external pi: ([@bs.as (-1)] _) => t = "acos";
[@bs.send] external isFinite: t => bool = "isFinite";
[@bs.send] external cmp: (t, t) => int = "cmp";
[@bs.send] external eq: (t, t) => bool = "eq";
[@bs.send] external gt: (t, t) => bool = "gt";
[@bs.send] external gte: (t, t) => bool = "gte";
[@bs.send] external lt: (t, t) => bool = "lt";
[@bs.send] external lte: (t, t) => bool = "lte";
[@bs.module "./Decimal"] [@bs.scope "default"]
external inv: ([@bs.as 1] _, t) => t = "div";
[@bs.send] external neg: t => t = "neg";
[@bs.send] external abs: t => t = "abs";
[@bs.send] external acos: t => t = "acos";
[@bs.send] external acosh: t => t = "acosh";
[@bs.send] external add: (t, t) => t = "add";
[@bs.send] external asin: t => t = "asin";
[@bs.send] external asinh: t => t = "asinh";
[@bs.send] external atan: t => t = "atan";
[@bs.send] external atanh: t => t = "atanh";
[@bs.send] external cbrt: t => t = "cbrt";
[@bs.send] external ceil: t => t = "ceil";
[@bs.send] external cos: t => t = "cos";
[@bs.send] external cosh: t => t = "cosh";
[@bs.send] external div: (t, t) => t = "div";
[@bs.send] external exp: t => t = "exp";
[@bs.send] external floor: t => t = "floor";
[@bs.send] external ln: t => t = "ln";
[@bs.send] external log: (t, t) => t = "log";
[@bs.send] external rem: (t, t) => t = "mod";
[@bs.send] external mul: (t, t) => t = "mul";
[@bs.send] external pow: (t, t) => t = "pow";
[@bs.send] external random: unit => t = "random";
[@bs.send] external round: t => t = "round";
[@bs.send] external sign: t => t = "sign";
[@bs.send] external sin: t => t = "sin";
[@bs.send] external sinh: t => t = "sinh";
[@bs.send] external sqrt: t => t = "sqrt";
[@bs.send] external sub: (t, t) => t = "sub";
[@bs.send] external tan: t => t = "tan";
[@bs.send] external tanh: t => t = "tanh";
[@bs.send] external trunc: t => t = "trunc";
[@bs.module "./Decimal"] [@bs.scope "default"]
external atan2: (t, t) => t = "atan2";
[@bs.module "./Decimal"] [@bs.scope "default"] external log2: t => t = "log2";
[@bs.module "./Decimal"] [@bs.scope "default"]
external log10: t => t = "log10";
// [@bs.module "./Decimal"] [@bs.scope "default"] [@bs.variadic]
// external hypot: array(t) => t = "hypot";
// [@bs.module "./Decimal"] [@bs.scope "default"] [@bs.variadic]
// external max: array(t) => t = "max";
// [@bs.module "./Decimal"] [@bs.scope "default"] [@bs.variadic]
// external min: array(t) => t = "min";

[@bs.send] external (==): (t, t) => bool = "eq";
let (!=) = (a, b) => !(a == b);
[@bs.send] external (>): (t, t) => bool = "gt";
[@bs.send] external (>=): (t, t) => bool = "gte";
[@bs.send] external (<): (t, t) => bool = "lt";
[@bs.send] external (<=): (t, t) => bool = "lte";
[@bs.send] external (~-): t => t = "neg";
[@bs.send] external (+): (t, t) => t = "add";
[@bs.send] external (-): (t, t) => t = "sub";
[@bs.send] external ( * ): (t, t) => t = "mul";
[@bs.send] external (/): (t, t) => t = "div";
[@bs.send] external ( ** ): (t, t) => t = "pow";
[@bs.send] external (mod): (t, t) => t = "mod";
