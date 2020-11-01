open Value_Base;

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

 One optimisation we do is we record both the x and f(x) of the previous
 iteration. Should
 */

type previous = {
  xPrev: float,
  fxPrev: float,
};

let solveRoot = (f, initial) => {
  let precision = 1e-8;
  let optimiseGradientLimit = 2.;

  let rec bisectFloat = (~iterations=100, ~negativeX, ~positiveX, ()) => {
    let mFloat = (negativeX +. positiveX) /. 2.0;
    let m = ofFloat(mFloat);
    let fm = f(m);
    let fmFloat = toDecimal(fm)->Decimal.toFloat;
    if (abs_float(fmFloat) < precision) {
      m;
    } else if (iterations > 0) {
      let iterations = iterations - 1;
      if (fmFloat > 0.) {
        bisectFloat(~iterations, ~negativeX, ~positiveX=mFloat, ());
      } else {
        bisectFloat(~iterations, ~negativeX=mFloat, ~positiveX, ());
      };
    } else {
      nan;
    };
  };

  let rec newtonFloat = (~iterations=100, ~fxReal=?, ~previous=None, x, ()) => {
    let xReal = ofFloat(x);
    let fxReal =
      switch (fxReal) {
      | Some(fxReal) => fxReal
      | None => f(xReal)
      };
    let fx = fxReal->toDecimal->Decimal.toFloat;
    if (abs_float(fx) < precision) {
      xReal;
    } else if (iterations > 0) {
      /*
       Not calling bisect on each branch here - and only calling it once below -
       means we can avoid creating a closure (check the JS output)
       */
      let op =
        switch (previous) {
        | Some({xPrev, fxPrev}) when fx > 0. && fxPrev < 0. =>
          `Bisect((xPrev, x))
        | Some({xPrev, fxPrev}) when fx < 0. && fxPrev > 0. =>
          `Bisect((x, xPrev))
        | Some({xPrev, fxPrev})
            when
              abs_float(fx -. fxPrev) < optimiseGradientLimit
              && abs_float(x -. xPrev) < optimiseGradientLimit =>
          `Gradient((fx -. fxPrev) /. (x -. xPrev))
        | _ =>
          `Gradient(
            Value_Calculus.derivative(f, xReal)->toDecimal->Decimal.toFloat,
          )
        };

      switch (op) {
      | `Bisect(negativeX, positiveX) =>
        bisectFloat(~negativeX, ~positiveX, ())
      | `Gradient(f'x) =>
        let iterations = iterations - 1;
        let previous = Some({xPrev: x, fxPrev: fx});

        if (f'x != 0.) {
          let xNext = x -. fx /. f'x;
          newtonFloat(~iterations, ~previous, xNext, ());
        } else {
          /* Steffensen's method */
          let gx = f(Value_Core.(xReal + fxReal))->toDecimal->Decimal.toFloat;
          if (gx != 0.) {
            let xNext = x -. fx /. gx;
            newtonFloat(~iterations, ~previous, xNext, ());
          } else {
            nan;
          };
        };
      };
    } else {
      nan;
    };
  };

  let newtonPrecise = x => {
    let fx = f(x);
    let f'x = Value_Calculus.derivative(f, x);
    let (xPrev, fxPrev) = (x, fx);
    let x = Value_Core.(x - fx / f'x);
    let fx = f(x);
    if (toDecimal(fx)->Decimal.toFloat->abs_float < precision) {
      x;
    } else {
      let previous =
        Some({
          xPrev: toDecimal(xPrev)->Decimal.toFloat,
          fxPrev: toDecimal(fxPrev)->Decimal.toFloat,
        });
      let x = toDecimal(x)->Decimal.toFloat;
      newtonFloat(~previous, ~fxReal=fx, x, ());
    };
  };

  newtonPrecise(initial);
};
