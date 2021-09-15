# TechniCalc Calculator

A math library similar to math-js, built in ReasonML. It supports complex numbers and matrices. Designed for use in the [TechniCalc app](https://apps.apple.com/gb/app/technicalc-calculator/id1504965415).

It uses rational numbers most of the time. When it is using rational numbers, it tracks some (Pi, sqrt() and exp()) values for more accurate results. For example, `sin(pi)` is exactly `1`, but this is not the output when using floating point arithmetic. In addition, special trig values can be output, like `sin(pi / 2)` giving `sqrt(2)/2`.

When a number cannot be expressed as a rational, we represent it as a decimal using the decimal.js library. When this happens, precision is lost, so it is hard to go back to the rational representation (although multiplying by zero will do it).

The final output can either be a fraction with up to one tracked value, or float.

The aim is to not be a symbolic calculator or cas, but to take some concepts from symbolic calculation to improve the accuracy.

## Type Scructure

- `Real.t` -> rational number (with optional constant) or decimal.js number (when rational numbers are not possible)
- `Scalar.Finite.t` -> `[ #Zero | #Real(Real.t) | #Imag(Real.t) | #Complex(Real.t, Real.t) ]`
- `Scalar.t` -> `[ Scalar.Finite.t | #NaN ]`
- `Vector.t` -> `array(Scalar.Finite.t)`
- `Matrix.t` -> `{ numRows: int, numColumns: int, elements: array(Scalar.Finite.t) }`
- `Value.t` -> `[ Scalar.t | #Percent(Scalar.Finite.t) | #Matrix(#Matrix.t) | #Vector(Vector.t) ]`

Note that all polymorphic variants are represented by 4 letters to make them unifrom in length, which can make large switch statements easier to read.

The scalar type will always simplify to the most compact representation. `Scalar.ofReal(Real.zero)` gives `#Zeroro`. Likewise, `Real.t` can represent nans, but `Scalar.t` will coerce these to `#NaN`. The same concept is used for all types, including `Vector.t`, `Matrix.t`, and `Value.t`. This is relied upon during pattern matching.

When using `Scalar.Finite.t`, you will be guaranteed any contained `Real.t`s will not be `NaN`.

For the above reasons, it is vital you never create your own values without using the constructor functions. I.e. use `Scalar.ofDecimal(...)` over `#Real(Real.Decimal(...))`.

---

### Compiling

To compile, `yarn build`, or `yarn start` to enter watch mode.

For performance reasons, in the production build, all tags in the type structure (aka `#Zeroro` or `#Zero`) are renamed to use integers instead of strings.

### Testing

To test, build in watch mode (`yarn start`), and in another terminal tab, `yarn test`. Testing is mostly fuzz testing against math-js and the native math library, and the tests can take minutes to complete.

If you want to run a quicker test suite, run `SAMPLE=20 yarn test`.

Collecting coverage is very useful to check which branches are called - especially the HTML outputs. However, as the JS files are compiled, an actual metric is not too useful. To get coverage, run `yarn test --coverage`, and check the `/coverage` folder.
