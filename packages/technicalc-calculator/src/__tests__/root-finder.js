const { toMatchJsValue } = require("../__test-util__");
const AST = require("../AST/AST.bs");

expect.extend({ toMatchJsValue });

it("solves sin(x) starting at 1", () => {
  const expr = AST.sin(AST.variable("x"));
  const start = AST.ofInt(1);
  const value = AST.solveRoot(expr, start);
  expect(value).toMatchJsValue(0);
});

it("solves sin(x) starting at 3", () => {
  const expr = AST.sin(AST.variable("x"));
  const start = AST.ofInt(3);
  const value = AST.solveRoot(expr, start);
  expect(value).toMatchJsValue(Math.PI);
});

it("solves x^5 - 2", () => {
  const expr = AST.sub(AST.pow(AST.variable("x"), AST.ofInt(5)), AST.ofInt(2));
  const start = AST.ofInt(1);
  const value = AST.solveRoot(expr, start);
  expect(value).toMatchJsValue(2 ** (1 / 5));
});

it("solves x^5 - 6", () => {
  const expr = AST.sub(AST.pow(AST.variable("x"), AST.ofInt(5)), AST.ofInt(6));
  const start = AST.ofInt(1);
  const value = AST.solveRoot(expr, start);
  expect(value).toMatchJsValue(6 ** (1 / 5));
});

it("solves x^5 - 6 starting at 1000", () => {
  const expr = AST.sub(AST.pow(AST.variable("x"), AST.ofInt(5)), AST.ofInt(6));
  const start = AST.ofInt(1000);
  const value = AST.solveRoot(expr, start);
  expect(value).toMatchJsValue(6 ** (1 / 5));
});

it("solves 6x^3 - 5x^2 - 17x + 6", () => {
  const a = AST.mul(AST.ofInt(6), AST.pow(AST.variable("x"), AST.ofInt(3)));
  const b = AST.mul(AST.ofInt(-5), AST.pow(AST.variable("x"), AST.ofInt(2)));
  const c = AST.mul(AST.ofInt(-17), AST.variable("x"));
  const d = AST.ofInt(6);
  const expr = [b, c, d].reduce(AST.add, a);
  const start = AST.ofInt(1000);
  const value = AST.solveRoot(expr, start);
  expect(value).toMatchJsValue(2);
});
