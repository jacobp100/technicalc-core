open Value_Base

/*
 Root finding!

 We start by using the Newton Raphson method.

 We do one attempt at NR using arbitrary precision rationals and constants (as
 does the rest of this library). However, this quickly becomes way too slow to
 continue using in practise. After one iteration of that, we switch to floats.

 NR gets us a good amount of the way; however, in certain scenarios we have to
 switch to other methods.

 Firstly, if we get a gradient of zero for an iteration (where x is non-zero),
 we use Steffensen's method instead. This has the same convergence as NR, but
 can pick roots far away from the origin

 If we experience a sign change when doing NR, we switch to bisection. For
 functions like sin, it's pretty much impossible to not get a root change.

 Interval bisection can fail if there is a discontinuity between the points the
 sign change occurred on. In this case, we revert back to NR/Steffensen's.

 One optimisation we do is we record both the x and f(x) of the previous
 iteration.
 */

type previous = {
  xPrev: Decimal.t,
  fxPrev: Decimal.t,
}

type iterationMode =
  | Bisect(Decimal.t, Decimal.t)
  | Gradient(Decimal.t)

%%private(let precision = Decimal.ofFloat(1e-8))
%%private(let optimiseGradientLimit = Decimal.ofInt(2))
%%private(
  let withinPrecision = x => {
    open Decimal
    abs(x) < precision
  }
)
%%private(
  let ltZero = x => {
    open Decimal
    x < zero
  }
)
%%private(
  let gtZero = x => {
    open Decimal
    x > zero
  }
)
%%private(
  let eqZero = x => {
    open Decimal
    x == zero
  }
)

%%private(
  let rec bisectDecimalU = (~iterations=100, ~negativeX, ~positiveX, f) => {
    let m = {
      open Decimal
      (positiveX + negativeX) / ofInt(2)
    }
    let mReal = ofDecimal(m)
    let fm = f(. mReal)->toDecimal

    if withinPrecision(fm) {
      mReal
    } else if iterations > 0 {
      let iterations = iterations - 1

      if gtZero(fm) {
        bisectDecimalU(~iterations, ~negativeX, ~positiveX=m, f)
      } else {
        bisectDecimalU(~iterations, ~negativeX=m, ~positiveX, f)
      }
    } else {
      nan
    }
  }
)

%%private(
  let rec newtonDecimalU = (~canBisect=true, ~iterations=100, ~previous, f, x) => {
    let xReal = ofDecimal(x)
    let fxReal = f(. xReal)
    let fx = toDecimal(fxReal)

    if withinPrecision(fx) {
      xReal
    } else if iterations > 0 {
      let {xPrev, fxPrev} = previous
      let op = if canBisect && gtZero(fx) && ltZero(fxPrev) {
        Bisect(xPrev, x)
      } else if canBisect && ltZero(fx) && gtZero(fxPrev) {
        Bisect(x, xPrev)
      } else {
        let xDelta = Decimal.sub(x, xPrev)
        let fxDelta = Decimal.sub(fx, fxPrev)

        let withinOptimiseGradientLimit = {
          open Decimal
          abs(fxDelta) < optimiseGradientLimit && abs(xDelta) < optimiseGradientLimit
        }

        if withinOptimiseGradientLimit {
          let gradient = Decimal.div(fxDelta, xDelta)
          Gradient(gradient)
        } else {
          Gradient(Value_Calculus.differentiateU(f, xReal)->toDecimal)
        }
      }

      switch op {
      | Bisect(negativeX, positiveX) =>
        let out = bisectDecimalU(~negativeX, ~positiveX, f)
        if !isNaN(out) {
          out
        } else {
          newtonDecimalU(~canBisect=false, ~iterations, ~previous, f, x)
        }
      | Gradient(f'x) =>
        let iterations = iterations - 1
        let previous = {xPrev: x, fxPrev: fx}

        if !eqZero(f'x) {
          let xNext = {
            open Decimal
            x - fx / f'x
          }
          newtonDecimalU(~iterations, ~previous, f, xNext)
        } else {
          let gx = f(. Value.add(xReal, fxReal))->toDecimal
          if !eqZero(gx) {
            let xNext = {
              open Decimal
              x - fx / gx
            }
            newtonDecimalU(~iterations, ~previous, f, xNext)
          } else {
            nan
          }
        }
      }
    } else {
      nan
    }
  }
)

%%private(
  let newtonPreciseU = (f, x) => {
    let fx = f(. x)
    let f'x = Value_Calculus.differentiateU(f, x)
    let (xPrev, fxPrev) = (x, fx)
    let x = {
      open Value
      x - fx / f'x
    }

    if toDecimal(fx)->withinPrecision {
      x
    } else {
      let previous = {
        xPrev: toDecimal(xPrev),
        fxPrev: toDecimal(fxPrev),
      }
      let x = toDecimal(x)
      newtonDecimalU(~previous, f, x)
    }
  }
)

let solveRootU = (f, initial) => newtonPreciseU(f, initial)
