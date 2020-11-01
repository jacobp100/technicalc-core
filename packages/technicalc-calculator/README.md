# TechniCalc Calculator

A math library similar to math-js, built in ReasonML. It supports complex numbers and matrices. Designed for use in the [TechniCalc app](https://apps.apple.com/gb/app/technicalc-calculator/id1504965415).

It uses rational numbers most of the time. When it is using rational numbers, it tracks some (Pi, sqrt() and exp()) values for more accurate results. For example, `sin(pi)` is exactly `1`, but this is not the output when using floating point arithmetic. In addition, special trig values can be output, like `sin(pi / 2)` giving `sqrt(2)/2`.

When a number cannot be expressed as a rational, we represent it as a decimal using the decimal.js library. When this happens, precision is lost, so it is hard to go back to the rational representation (although multiplying by zero will do it).

The final output can either be a fraction with up to one tracked value, or float.

The aim is to not be a symbolic calculator or cas, but to take some concepts from symbolic calculation to improve the accuracy.

## Type Scructure

- `Real.t` -> rational number (with optional constant) or decimal.js number (when rational numbers are not possible)
- `Vector.t` -> `array(Real.t)`
- `Matrix.t` -> `{ numRows: int, numColumns: int, elements: array(Real.t) }`
- `Scalar.t` -> `` [ `Zero | `Real(Real.t) | `Imag(Real.t) | `Complex(Real.t, Real.t) ] ``
- `Value.t` -> `` [ Scalar.t | `Percent(Real.t) | `Matrix(`Matrix.t) | `Vector(Vector.t) | `NaN ] ``

Note that all polymorphic variants are represented only using the first character. I.e. `` `Zero `` becomes `` `Z ``. This is because ReScript now represents these as strings, and using a single character improved performance by around 10%. Should the representation change such that the performance gain can be maintained, it should be pretty easy do to a find and replace to revert back.

The scalar type will always simplify to the most compact representation. `Scalar.ofReal(Real.zero)` gives `` `Zero ``. This is relied upon during pattern matching.

The same is true for the value type.

---

### Compiling

To compile, `yarn build`, or `yarn start` to enter watch mode.

### Testing

To test, build in watch mode (`yarn start`), and in another terminal tab, `yarn test`. Testing is mostly fuzz testing against math-js and the native math library, and the tests can take minutes to complete.

If you want to run a quicker test suite, run `SAMPLE=20 yarn test`.

Collecting coverage is very useful to check which branches are called - especially the HTML outputs. However, as the JS files are compiled, an actual metric is not too useful. To get coverage, run `yarn test --coverage`, and check the `/coverage` folder.
