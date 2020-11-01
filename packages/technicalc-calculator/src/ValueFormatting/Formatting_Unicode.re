let pi = {j|π|j};
let sqrt = {j|√|j};
let divide = {j|÷|j};
let magnitude = {j|×10|j};

let formatSuperscriptNumbers = str => {
  Belt.Array.makeByU(String.length(str), (. i) => {
    switch (StringUtil.stringCharAtUnsafe(str, i)) {
    | "-" => {j|⁻|j}
    | "0" => {j|⁰|j}
    | "1" => {j|¹|j}
    | "2" => {j|²|j}
    | "3" => {j|³|j}
    | "4" => {j|⁴|j}
    | "5" => {j|⁵|j}
    | "6" => {j|⁶|j}
    | "7" => {j|⁷|j}
    | "8" => {j|⁸|j}
    | "9" => {j|⁹|j}
    | x => x
    }
  })
  ->StringUtil.join;
};
