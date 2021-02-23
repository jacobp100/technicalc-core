open Jest;

test("possible solver bug report", (.) => {
  open AST;
  let x = Variable("x");
  let ofFloat = a => OfFloat(a);
  let (+) = (a, b) => Add(a, b);
  let (-) = (a, b) => Sub(a, b);
  let ( * ) = (a, b) => Mul(a, b);
  // let (/) = (a, b) => Div(a, b);
  let ( ** ) = (a, b) => Pow(a, b);

  let equation =
    ofFloat(18.75)
    - ofFloat(1. /. 4.)
    * x
    ** ofFloat(2.)
    + ofFloat(2.5)
    * x
    + (x + ofFloat(5.))
    * ofFloat(1. /. 2.)
    * (ofFloat(-1.) * x + ofFloat(2.5));

  let root =
    AST.solveRoot(
      ~config=defaultConfig,
      ~context=AST_Context.empty,
      equation,
      ofFloat(0.),
    );

  expect(Value.toFloat(root))->toBe(-5.);
});

test("solver bug report 2", (.) => {
  open AST;
  let x = Variable("x");
  let ofInt = a => OfInt(a);
  let (+) = (a, b) => Add(a, b);
  let (-) = (a, b) => Sub(a, b);
  let ( * ) = (a, b) => Mul(a, b);
  let (/) = (a, b) => Div(a, b);
  let ( ** ) = (a, b) => Pow(a, b);

  let equation =
    (ofInt(3) * x - (x + ofInt(3)) * ofInt(3))
    / (ofInt(9) * x ** ofInt(2))
    + ofInt(1);

  let root =
    AST.solveRoot(
      ~config=defaultConfig,
      ~context=AST_Context.empty,
      equation,
      ofInt(1),
    );

  expect(Value.toFloat(root))->toBe(1.);
});
