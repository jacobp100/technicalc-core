type t;
[@bs.new] [@bs.module "../../decimal/decimal"]
external ofInt: int => t = "default";
[@bs.new] [@bs.module "../../decimal/decimal"]
external ofFloat: float => t = "default";
[@bs.new] [@bs.module "../../decimal/decimal"]
external ofString: string => t = "default";
[@bs.send] external toFloat: t => float = "toNumber";
[@bs.send] external toBinary: t => string = "toBinary";
[@bs.send] external toOctal: t => string = "toOctal";
[@bs.send] external toHexadecimal: t => string = "toHexadecimal";
[@bs.send] external toString: t => string = "toString";
let zero = ofInt(0);
let one = ofInt(1);
let minusOne = ofInt(-1);
let nan = ofFloat(nan);
[@bs.module "../../decimal/decimal"]
external pi: ([@bs.as (-1)] _) => t = "acos";
[@bs.module "../../decimal/decimal"] external isFinite: t => bool = "isFinite";
[@bs.module "../../decimal/decimal"] external cmp: (t, t) => int = "cmp";
[@bs.module "../../decimal/decimal"] external eq: (t, t) => bool = "eq";
[@bs.module "../../decimal/decimal"] external gt: (t, t) => bool = "gt";
[@bs.module "../../decimal/decimal"] external gte: (t, t) => bool = "gte";
[@bs.module "../../decimal/decimal"] external lt: (t, t) => bool = "lt";
[@bs.module "../../decimal/decimal"] external lte: (t, t) => bool = "lte";
[@bs.module "../../decimal/decimal"]
external inv: ([@bs.as 1] _, t) => t = "div";
[@bs.module "../../decimal/decimal"] external neg: t => t = "neg";
[@bs.module "../../decimal/decimal"] external abs: t => t = "abs";
[@bs.module "../../decimal/decimal"] external acos: t => t = "acos";
[@bs.module "../../decimal/decimal"] external acosh: t => t = "acosh";
[@bs.module "../../decimal/decimal"] external add: (t, t) => t = "add";
[@bs.module "../../decimal/decimal"] external asin: t => t = "asin";
[@bs.module "../../decimal/decimal"] external asinh: t => t = "asinh";
[@bs.module "../../decimal/decimal"] external atan: t => t = "atan";
[@bs.module "../../decimal/decimal"] external atanh: t => t = "atanh";
[@bs.module "../../decimal/decimal"] external cbrt: t => t = "cbrt";
[@bs.module "../../decimal/decimal"] external ceil: t => t = "ceil";
[@bs.module "../../decimal/decimal"] external cos: t => t = "cos";
[@bs.module "../../decimal/decimal"] external cosh: t => t = "cosh";
[@bs.module "../../decimal/decimal"] external div: (t, t) => t = "div";
[@bs.module "../../decimal/decimal"] external exp: t => t = "exp";
[@bs.module "../../decimal/decimal"] external floor: t => t = "floor";
[@bs.module "../../decimal/decimal"] external ln: t => t = "ln";
[@bs.module "../../decimal/decimal"] external log: (t, t) => t = "log";
[@bs.module "../../decimal/decimal"] external rem: (t, t) => t = "mod";
[@bs.module "../../decimal/decimal"] external mul: (t, t) => t = "mul";
[@bs.module "../../decimal/decimal"] external pow: (t, t) => t = "pow";
[@bs.module "../../decimal/decimal"] external random: unit => t = "random";
[@bs.module "../../decimal/decimal"] external round: t => t = "round";
[@bs.module "../../decimal/decimal"] external sign: t => t = "sign";
[@bs.module "../../decimal/decimal"] external sin: t => t = "sin";
[@bs.module "../../decimal/decimal"] external sinh: t => t = "sinh";
[@bs.module "../../decimal/decimal"] external sqrt: t => t = "sqrt";
[@bs.module "../../decimal/decimal"] external sub: (t, t) => t = "sub";
[@bs.module "../../decimal/decimal"] external tan: t => t = "tan";
[@bs.module "../../decimal/decimal"] external tanh: t => t = "tanh";
[@bs.module "../../decimal/decimal"] external trunc: t => t = "trunc";
[@bs.module "../../decimal/decimal"] external atan2: (t, t) => t = "atan2";
[@bs.module "../../decimal/decimal"] external log2: t => t = "log2";
[@bs.module "../../decimal/decimal"] external log10: t => t = "log10";
// [@bs.module "../../decimal/decimal"] [@bs.scope "default"] [@bs.variadic]
// external hypot: array(t) => t = "hypot";
// [@bs.module "../../decimal/decimal"] [@bs.scope "default"] [@bs.variadic]
// external max: array(t) => t = "max";
// [@bs.module "../../decimal/decimal"] [@bs.scope "default"] [@bs.variadic]
// external min: array(t) => t = "min";

[@bs.module "../../decimal/decimal"] external (==): (t, t) => bool = "eq";
let (!=) = (a, b) => !(a == b);
[@bs.module "../../decimal/decimal"] external (>): (t, t) => bool = "gt";
[@bs.module "../../decimal/decimal"] external (>=): (t, t) => bool = "gte";
[@bs.module "../../decimal/decimal"] external (<): (t, t) => bool = "lt";
[@bs.module "../../decimal/decimal"] external (<=): (t, t) => bool = "lte";
[@bs.module "../../decimal/decimal"] external (~-): t => t = "neg";
[@bs.module "../../decimal/decimal"] external (+): (t, t) => t = "add";
[@bs.module "../../decimal/decimal"] external (-): (t, t) => t = "sub";
[@bs.module "../../decimal/decimal"] external ( * ): (t, t) => t = "mul";
[@bs.module "../../decimal/decimal"] external (/): (t, t) => t = "div";
[@bs.module "../../decimal/decimal"] external ( ** ): (t, t) => t = "pow";
[@bs.module "../../decimal/decimal"] external (mod): (t, t) => t = "mod";
