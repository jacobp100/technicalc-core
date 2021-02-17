import _ from "lodash";
import * as mathjs from "mathjs/lib/esm";
import baseCartesian from "cartesian";
import * as TechniCalc from "../Value";
import * as TechniCalcTest from "../ValueTestUtil";
import sample from "./sample";

export class Value {
  constructor(jsValue, techniCalcValue, title = String(jsValue)) {
    this.title = title;
    this.techniCalcValue = techniCalcValue;
    this.jsValue = jsValue;
  }

  static float(n, title = String(n)) {
    return new Value(n, TechniCalc.ofFloat(n), title);
  }

  static complex(re, im, title = `${re}+${im}i`) {
    return new Value(
      mathjs.complex(re, im),
      TechniCalcTest.ofComplexFloats(re, im),
      title
    );
  }

  static e(n = 1, title = `${n}e`) {
    return new Value(
      n * Math.E,
      TechniCalc.mul(TechniCalc.ofFloat(n), TechniCalc.e),
      title
    );
  }

  static pi(n = 1, title = `${n}pi`) {
    return new Value(
      n * Math.PI,
      TechniCalc.mul(TechniCalc.ofFloat(n), TechniCalc.pi),
      title
    );
  }

  toString() {
    return this.title;
  }
}

export const range12 = _.range(1, 12 + 1);

const existingFractions = new Set();
export const fractionsTo12 = sample(
  baseCartesian([[0, ...range12], range12]).filter(([a, b]) => {
    const key = (a / b).toFixed(8);
    if (!existingFractions.has(key)) {
      existingFractions.add(key);
      return true;
    }
    return false;
  })
);

const isCloseTo = (a, b) => {
  if (Number.isNaN(a)) return Number.isNaN(b);
  if (!Number.isFinite(a)) return !Number.isFinite(b);

  const aMagnitude = Math.floor(Math.log10(Math.abs(a)));
  const bMagnitude = Math.floor(Math.log10(Math.abs(b)));

  const normalizerMagnitude =
    Math.abs(aMagnitude - bMagnitude) <= 1
      ? Math.max(1, Math.min(aMagnitude - 2, bMagnitude - 2))
      : 1;
  const normalizer = 10 ** normalizerMagnitude;

  return Math.abs(a - b) / normalizer < 1e-8;
};

const asComplex = (a) => {
  const comp = typeof a === "number" ? [a, 0] : [a.re, a.im];
  return !Number.isFinite(comp[0]) || !Number.isFinite(comp[1])
    ? [NaN, NaN]
    : comp;
};

export const toMatchJsValue = (received, expected, message) => {
  const [actualRe, actualIm] = TechniCalcTest.toComplexFloats(received);
  const [expectedRe, expectedIm] = asComplex(expected);

  const pass =
    isCloseTo(actualRe, expectedRe) && isCloseTo(actualIm, expectedIm);

  return {
    message: () =>
      `expected ${TechniCalcTest.toString(received)} ${
        pass ? "not " : ""
      }to be close to ${expectedRe}+${expectedIm}i (${
        message?.() ?? "no equation given"
      })`,
    pass,
  };
};

export const toMatchJsMatrix = (received, expected, message) => {
  const techniCalcElements = TechniCalcTest.toComplexFloatsMatrix(received);

  let allPass = true;
  expected.forEach((mathJsElement, [row, column]) => {
    const [actualRe, actualIm] = techniCalcElements[row][column];
    const [expectedRe, expectedIm] = asComplex(mathJsElement);
    const pass =
      isCloseTo(actualRe, expectedRe) && isCloseTo(actualIm, expectedIm);
    allPass = allPass && pass;
  });

  return {
    message: () =>
      `expected ${TechniCalcTest.toString(received)} ${
        allPass ? "not " : ""
      }to be close to ${expected} (${message?.() ?? "no equation given"})`,
    pass: allPass,
  };
};
