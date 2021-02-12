type t;

external ofInt: int => t = "%identity";
[@bs.module "./SafeInt.js"] external toInt: t => option(int) = "toInt";
[@bs.module "./SafeInt.js"] external abs: t => t = "abs";
[@bs.module "./SafeInt.js"] external neg: t => t = "neg";
[@bs.module "./SafeInt.js"] external add: (t, t) => t = "add";
[@bs.module "./SafeInt.js"] external sub: (t, t) => t = "sub";
[@bs.module "./SafeInt.js"] external mul: (t, t) => t = "mul";
[@bs.module "./SafeInt.js"] external div: (t, t) => t = "div";
[@bs.module "./SafeInt.js"] external pow: (t, t) => t = "pow";
[@bs.module "./SafeInt.js"] external rem: (t, t) => t = "mod";
[@bs.module "./SafeInt.js"] external (~-): t => t = "neg";
[@bs.module "./SafeInt.js"] external (+): (t, t) => t = "add";
[@bs.module "./SafeInt.js"] external (-): (t, t) => t = "sub";
[@bs.module "./SafeInt.js"] external ( * ): (t, t) => t = "mul";
[@bs.module "./SafeInt.js"] external (/): (t, t) => t = "div";
[@bs.module "./SafeInt.js"] external ( ** ): (t, t) => t = "pow";
[@bs.module "./SafeInt.js"] external (mod): (t, t) => t = "mod";

let negInt = a => ofInt(a)->neg->toInt;
let absInt = a => ofInt(a)->abs->toInt;
let addInt = (a, b) => (ofInt(a) + ofInt(b))->toInt;
let subInt = (a, b) => (ofInt(a) - ofInt(b))->toInt;
let mulInt = (a, b) => (ofInt(a) * ofInt(b))->toInt;
let divInt = (a, b) => (ofInt(a) / ofInt(b))->toInt;
let modInt = (a, b) => (ofInt(a) mod ofInt(b))->toInt;
