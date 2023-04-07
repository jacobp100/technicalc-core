let pi = `π`
let sqrt = `√`
let multiply = `×`
let divide = `÷`
let mu = `μ`
let omegaUpper = `Ω`
let degree = `°`

let formatSuperscriptNumbers = str =>
  Belt.Array.makeByU(String.length(str), (. i) =>
    switch StringUtil.stringCharAtUnsafe(str, i) {
    | "-" => `⁻`
    | "0" => `⁰`
    | "1" => `¹`
    | "2" => `²`
    | "3" => `³`
    | "4" => `⁴`
    | "5" => `⁵`
    | "6" => `⁶`
    | "7" => `⁷`
    | "8" => `⁸`
    | "9" => `⁹`
    | x => x
    }
  )->StringUtil.join
