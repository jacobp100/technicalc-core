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
  xPrev: float,
  fxPrev: float,
}

type iterationMode =
  | Bisect(float, float)
  | Gradient(float)

%%private(let precision = 1e-8)
%%private(let optimiseGradientLimit = 2.)

%%private(
  let rec bisectFloat = (~iterations=100, ~negativeX, ~positiveX, f) => {
    let mFloat = (negativeX +. positiveX) /. 2.0
    let m = ofFloat(mFloat)
    let fm = f(m)
    let fmFloat = toDecimal(fm)->Decimal.toFloat
    if FloatUtil.abs(fmFloat) < precision {
      m
    } else if iterations > 0 {
      let iterations = iterations - 1
      if fmFloat > 0. {
        bisectFloat(~iterations, ~negativeX, ~positiveX=mFloat, f)
      } else {
        bisectFloat(~iterations, ~negativeX=mFloat, ~positiveX, f)
      }
    } else {
      nan
    }
  }
)

%%private(
  let rec newtonFloat = (~canBisect=true, ~iterations=100, ~previous=None, f, x) => {
    let xReal = ofFloat(x)
    let fxReal = f(xReal)
    let fx = fxReal->toDecimal->Decimal.toFloat

    if FloatUtil.abs(fx) < precision {
      xReal
    } else if iterations > 0 {
      let op = switch previous {
      | Some({xPrev, fxPrev}) if canBisect && (fx > 0. && fxPrev < 0.) => Bisect(xPrev, x)
      | Some({xPrev, fxPrev}) if canBisect && (fx < 0. && fxPrev > 0.) => Bisect(x, xPrev)
      | Some({xPrev, fxPrev})
        if FloatUtil.abs(fx -. fxPrev) < optimiseGradientLimit &&
          FloatUtil.abs(x -. xPrev) < optimiseGradientLimit =>
        Gradient((fx -. fxPrev) /. (x -. xPrev))
      | _ => Gradient(Value_Calculus.differentiate(f, xReal)->toDecimal->Decimal.toFloat)
      }

      switch op {
      | Bisect(negativeX, positiveX) =>
        let out = bisectFloat(~negativeX, ~positiveX, f)
        if !isNaN(out) {
          out
        } else {
          newtonFloat(~canBisect=false, ~iterations, ~previous, f, x)
        }
      | Gradient(f'x) =>
        let iterations = iterations - 1
        let previous = Some({xPrev: x, fxPrev: fx})

        if f'x != 0. {
          let xNext = x -. fx /. f'x
          newtonFloat(~iterations, ~previous, f, xNext)
        } else {
          let gx = f(Value.add(xReal, fxReal))->toDecimal->Decimal.toFloat
          if gx != 0. {
            let xNext = x -. fx /. gx
            newtonFloat(~iterations, ~previous, f, xNext)
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
  let newtonPrecise = (f, x) => {
    let fx = f(x)
    let f'x = Value_Calculus.differentiate(f, x)
    let (xPrev, fxPrev) = (x, fx)
    let x = {
      open Value
      x - fx / f'x
    }
    if f(x)->toDecimal->Decimal.toFloat->FloatUtil.abs < precision {
      x
    } else {
      let previous = Some({
        xPrev: toDecimal(xPrev)->Decimal.toFloat,
        fxPrev: toDecimal(fxPrev)->Decimal.toFloat,
      })
      let x = toDecimal(x)->Decimal.toFloat
      newtonFloat(~previous, f, x)
    }
  }
)

let solveRoot = (f, initial) => newtonPrecise(f, initial)
