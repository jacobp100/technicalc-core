let pi = j`π`
let sqrt = j`√`
let divide = j`÷`
let magnitude = j`×10`

let formatSuperscriptNumbers = str =>
  Belt.Array.makeByU(String.length(str), (. i) =>
    switch StringUtil.stringCharAtUnsafe(str, i) {
    | "-" => j`⁻`
    | "0" => j`⁰`
    | "1" => j`¹`
    | "2" => j`²`
    | "3" => j`³`
    | "4" => j`⁴`
    | "5" => j`⁵`
    | "6" => j`⁶`
    | "7" => j`⁷`
    | "8" => j`⁸`
    | "9" => j`⁹`
    | x => x
    }
  )->StringUtil.join
