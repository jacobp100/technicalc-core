import {
  ofFloat,
  pi,
  add,
  mul,
  div,
  sqrt,
  log,
  exp,
  sin,
} from "../Value/Value";
import { toString } from "../__test-util__/ValueTestUtil";

test("simplifies division by two square roots", () => {
  expect(toString(div(sqrt(ofFloat(10)), sqrt(ofFloat(2))))).toEqual("sqrt(5)");
  expect(toString(div(sqrt(ofFloat(1000)), sqrt(ofFloat(2))))).toEqual(
    "10sqrt(5)"
  );
});

test("tracks exp values through log", () => {
  expect(toString(log(exp(ofFloat(47))))).toEqual("47");
});

test("simplifies square roots and exponentials", () => {
  expect(toString(mul(ofFloat(2), sqrt(ofFloat(2))))).toEqual("2sqrt(2)");
  expect(toString(sqrt(ofFloat(1000)))).toEqual("10sqrt(10)");
  expect(toString(sqrt(ofFloat(4)))).toEqual("2");
  expect(toString(sqrt(ofFloat(8)))).toEqual("2sqrt(2)");
  expect(toString(sqrt(ofFloat(6)))).toEqual("sqrt(6)");
  expect(toString(sqrt(ofFloat(12)))).toEqual("2sqrt(3)");
  expect(toString(sqrt(ofFloat(0)))).toEqual("0");
  expect(toString(exp(ofFloat(0)))).toEqual("1");
  expect(toString(exp(ofFloat(1)))).toEqual("exp(1)");
  expect(toString(exp(ofFloat(2)))).toEqual("exp(2)");
  expect(toString(exp(ofFloat(3)))).toEqual("exp(3)");
  expect(toString(exp(ofFloat(-1)))).toEqual("exp(-1)");
});

test("does not simplify pi", () => {
  expect(toString(pi)).toEqual("pi");
});

test("takes sin of pi + 1", () => {
  expect(toString(sin(add(pi, ofFloat(1))))).toBe("-0.841470984807");
});
