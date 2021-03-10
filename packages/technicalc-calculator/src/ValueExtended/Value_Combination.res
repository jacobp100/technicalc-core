open Value_Factorial
open Value_Core

let nPr = (n, r) => n->factorial / (n - r)->factorial

let nCr = (n, r) => n->factorial / (r->factorial * (n - r)->factorial)
