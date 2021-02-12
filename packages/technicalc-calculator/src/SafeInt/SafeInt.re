type t;

external ofInt: int => t = "%identity";
[@bs.module "./_SafeInt.mjs"] external toInt: t => option(int) = "toInt";
[@bs.module "./_SafeInt.mjs"] external abs: t => t = "abs";
[@bs.module "./_SafeInt.mjs"] external neg: t => t = "neg";
[@bs.module "./_SafeInt.mjs"] external add: (t, t) => t = "add";
[@bs.module "./_SafeInt.mjs"] external sub: (t, t) => t = "sub";
[@bs.module "./_SafeInt.mjs"] external mul: (t, t) => t = "mul";
[@bs.module "./_SafeInt.mjs"] external div: (t, t) => t = "div";
[@bs.module "./_SafeInt.mjs"] external pow: (t, t) => t = "pow";
[@bs.module "./_SafeInt.mjs"] external rem: (t, t) => t = "mod";
[@bs.module "./_SafeInt.mjs"] external (~-): t => t = "neg";
[@bs.module "./_SafeInt.mjs"] external (+): (t, t) => t = "add";
[@bs.module "./_SafeInt.mjs"] external (-): (t, t) => t = "sub";
[@bs.module "./_SafeInt.mjs"] external ( * ): (t, t) => t = "mul";
[@bs.module "./_SafeInt.mjs"] external (/): (t, t) => t = "div";
[@bs.module "./_SafeInt.mjs"] external ( ** ): (t, t) => t = "pow";
[@bs.module "./_SafeInt.mjs"] external (mod): (t, t) => t = "mod";

let negInt = a => ofInt(a)->neg->toInt;
let absInt = a => ofInt(a)->abs->toInt;
let addInt = (a, b) => (ofInt(a) + ofInt(b))->toInt;
let subInt = (a, b) => (ofInt(a) - ofInt(b))->toInt;
let mulInt = (a, b) => (ofInt(a) * ofInt(b))->toInt;
let divInt = (a, b) => (ofInt(a) / ofInt(b))->toInt;
let modInt = (a, b) => (ofInt(a) mod ofInt(b))->toInt;
