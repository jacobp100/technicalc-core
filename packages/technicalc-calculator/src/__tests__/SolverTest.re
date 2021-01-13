open Jest;

test("possible solver bug report", (.) => {
  open AST_Types;
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

  let root = AST.solveRoot(equation, ofFloat(0.));

  expect(Value.toFloat(root))->toBe(-5.);
});
