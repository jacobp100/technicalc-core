open Value_Factorial;

let nPr = (n, r) => Value_Core.(n->factorial / (n - r)->factorial);

let nCr = (n, r) =>
  Value_Core.(n->factorial / (r->factorial * (n - r)->factorial));
