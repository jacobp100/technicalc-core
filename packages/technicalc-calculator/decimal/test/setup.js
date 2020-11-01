// Adds global: T

import Decimal from "../decimal";
import * as fns from "../decimal";

global.Decimal = Decimal;

[
  "abs",
  "acos",
  "acosh",
  "add",
  "asin",
  "asinh",
  "atan",
  "atanh",
  "atan2",
  "cbrt",
  "ceil",
  "cos",
  "cosh",
  "div",
  "exp",
  "floor",
  "hypot",
  "ln",
  "log",
  "log10",
  "log2",
  "max",
  "min",
  "mod",
  "mul",
  "pow",
  "random",
  "round",
  "sign",
  "sin",
  "sinh",
  "sqrt",
  "sub",
  "tan",
  "tanh",
  "trunc",
  "divToInt",
  "eq",
  "gt",
  "lt",
  "gte",
  "lte",
  "cmp",
  "dp",
  "sd",
  "isInt",
  "isFinite",
  "isNaN",
  "isNeg",
  "isZero",
  "isDecimal",
  "toSD",
].forEach((fn) => {
  Decimal[fn] = fns[fn];
  Decimal.prototype[fn] = function (...args) {
    return fns[fn](this, ...args);
  };
  Decimal.prototype.valueOf = function () {
    return fns.toJSON(this);
  };
  Decimal.prototype.comparedTo = Decimal.prototype.cmp;
  Decimal.prototype.equals = Decimal.prototype.eq;
  Decimal.prototype.greaterThan = Decimal.prototype.gt;
  Decimal.prototype.lessThan = Decimal.prototype.lt;
  Decimal.prototype.greaterThanOrEqualTo = Decimal.prototype.gte;
  Decimal.prototype.lessThanOrEqualTo = Decimal.prototype.lte;
  Decimal.prototype.isNegative = Decimal.prototype.isNeg;
  Decimal.prototype.isInteger = Decimal.prototype.isInt;
  Decimal.prototype.decimalPlaces = Decimal.prototype.dp;
});

global.T = (function () {
  var passed, testNumber, write;

  function T(name, tests) {
    var time;
    write(" Testing " + name + "...");
    passed = testNumber = 0;
    time = new Date();
    try {
      tests();
    } catch (e) {
      console.log("\nFailed to run " + name);
      console.log(e);
    }
    T.result = [passed, testNumber, time];
    time = new Date() - time;
    if (passed !== testNumber) write("\n");
    write(
      " " + passed + " of " + testNumber + " tests passed in " + time + " ms\n"
    );
  }

  if (typeof window != "undefined") {
    write = function (str) {
      document.body.innerHTML += str
        .replace(/\n/g, "<br>")
        .replace(/ /g, "&nbsp;");
    };
  } else {
    // Decimal = require("../decimal");
    write = process.stdout.write.bind(process.stdout);
  }

  T.assert = function (actual) {
    ++testNumber;
    if (actual === true) {
      ++passed;
      //write('\n Expected and actual: ' + actual);
    } else {
      write(
        "\n  Test number " +
          testNumber +
          " failed: assert" +
          "\n  Expected: true" +
          "\n  Actual:   " +
          actual
      );
      //process.exit();
    }
  };

  T.assertEqual = function (expected, actual) {
    ++testNumber;
    // If expected and actual are both NaN, consider them equal.
    if (expected === actual || (expected !== expected && actual !== actual)) {
      ++passed;
    } else {
      write(
        "\n  Test number " +
          testNumber +
          " failed: assertEqual" +
          "\n  Expected: " +
          expected +
          "\n  Actual:   " +
          actual
      );
    }
  };

  T.assertEqualDecimal = function (x, y) {
    ++testNumber;
    if (x.eq(y) || (x.isNaN() && y.isNaN())) {
      ++passed;
    } else {
      write(
        "\n  Test number " +
          testNumber +
          " failed: assertEqualDecimal" +
          "\n  x: " +
          x.valueOf() +
          "\n  y: " +
          y.valueOf()
      );
    }
  };

  T.assertEqualProps = function (digits, exponent, sign, n) {
    var i = 0,
      len = digits.length;
    ++testNumber;
    while (i < len && digits[i] === n.d[i]) ++i;
    if (i === len && i === n.d.length && exponent === n.e && sign === n.s) {
      ++passed;
    } else {
      write(
        "\n  Test number " +
          testNumber +
          " failed: assertEqualProps" +
          "\n  Expected digits:   " +
          digits +
          "\n  Expected exponent: " +
          exponent +
          "\n  Expected sign:     " +
          sign +
          "\n  Actual digits:     " +
          n.d +
          "\n  Actual exponent:   " +
          n.e +
          "\n  Actual sign:       " +
          n.s
      );
    }
  };

  T.assertException = function (func, msg) {
    var actual;
    ++testNumber;
    try {
      func();
    } catch (e) {
      actual = e;
    }
    if (actual instanceof Error && /DecimalError/.test(actual.message)) {
      ++passed;
    } else {
      write(
        "\n  Test number " +
          testNumber +
          " failed: assertException" +
          "\n  Expected: " +
          msg +
          " to raise a DecimalError." +
          "\n  Actual:   " +
          (actual || "no exception")
      );
    }
  };

  return T;
})();
