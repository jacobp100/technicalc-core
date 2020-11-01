open AST_Types;

let rec eval = (~context, node: t): Value.t =>
  switch (node) {
  | NaN => `N
  | Zero => Value.zero
  | One => Value.one
  | MinusOne => Value.minusOne
  | I => Value.i
  | MinusI => Value.minusI
  | Pi => Value.pi
  | E => Value.e
  | OfInt(a) => Value.ofInt(a)
  | OfFloat(a) => Value.ofFloat(a)
  | OfString(a) => Value.ofString(a)->Belt.Option.getWithDefault(`N)
  | OfStringBase(base, a) =>
    Value.ofStringBase(base, a)->Belt.Option.getWithDefault(`N)
  | Percent(percent) => evalScalar(~context, percent)->Value.ofPercent
  | Vector(elements) =>
    Value.ofVector(Belt.Array.map(elements, evalScalar(~context)))
  | Matrix({numRows, numColumns, elements}) =>
    Matrix.{
      numRows,
      numColumns,
      elements: Belt.Array.map(elements, evalScalar(~context)),
    }
    ->Value.ofMatrix
  | OfEncoded(a) => Value.decode(a)->Belt.Option.getWithDefault(`N)
  | Variable(ident) => Belt.Map.String.getWithDefault(context, ident, `N)
  | Add(a, b) => Value.add(eval(~context, a), eval(~context, b))
  | Sub(a, b) => Value.sub(eval(~context, a), eval(~context, b))
  | Mul(a, b) => Value.mul(eval(~context, a), eval(~context, b))
  | Div(a, b) => Value.div(eval(~context, a), eval(~context, b))
  | Pow(a, b) => Value.pow(eval(~context, a), eval(~context, b))
  | Dot(a, b) => Value.dot(eval(~context, a), eval(~context, b))
  | Neg(a) => Value.neg(eval(~context, a))
  | Abs(a) => Value.abs(eval(~context, a))
  | Floor(a) => Value.floor(eval(~context, a))
  | Ceil(a) => Value.ceil(eval(~context, a))
  | Round(a) => Value.round(eval(~context, a))
  | Sqrt(a) => Value.sqrt(eval(~context, a))
  | Exp(a) => Value.exp(eval(~context, a))
  | Log(a) => Value.log(eval(~context, a))
  | Sin(a) => Value.sin(eval(~context, a))
  | Asin(a) => Value.asin(eval(~context, a))
  | Sinh(a) => Value.sinh(eval(~context, a))
  | Asinh(a) => Value.asinh(eval(~context, a))
  | Cos(a) => Value.cos(eval(~context, a))
  | Acos(a) => Value.acos(eval(~context, a))
  | Cosh(a) => Value.cosh(eval(~context, a))
  | Acosh(a) => Value.acosh(eval(~context, a))
  | Tan(a) => Value.tan(eval(~context, a))
  | Atan(a) => Value.atan(eval(~context, a))
  | Tanh(a) => Value.tanh(eval(~context, a))
  | Atanh(a) => Value.atanh(eval(~context, a))
  | Re(a) => Value.re(eval(~context, a))
  | Im(a) => Value.im(eval(~context, a))
  | Conj(a) => Value.conj(eval(~context, a))
  | Gamma(a) => Value.gamma(eval(~context, a))
  | Factorial(a) => Value.factorial(eval(~context, a))
  | Rand => Value.rand()
  | RandInt(a, b) => Value.randInt(eval(~context, a), eval(~context, b))
  | NPR(a, b) => Value.nPr(eval(~context, a), eval(~context, b))
  | NCR(a, b) => Value.nCr(eval(~context, a), eval(~context, b))
  | Min(a, b) => Value.min(eval(~context, a), eval(~context, b))
  | Max(a, b) => Value.max(eval(~context, a), eval(~context, b))
  | Gcd(a, b) => Value.gcd(eval(~context, a), eval(~context, b))
  | Lcm(a, b) => Value.lcm(eval(~context, a), eval(~context, b))
  | Differential({x, body}) =>
    Value.derivative(createEvalCb(~context, body), eval(~context, x))
  | Integral({a, b, body}) =>
    Value.integrate(
      createEvalCb(~context, body),
      eval(~context, a),
      eval(~context, b),
    )
  | Sum({a, b, body}) =>
    Value.sum(
      createEvalCb(~context, body),
      eval(~context, a),
      eval(~context, b),
    )
  | Product({a, b, body}) =>
    Value.product(
      createEvalCb(~context, body),
      eval(~context, a),
      eval(~context, b),
    )
  | Convert({a, fromUnits, toUnits}) =>
    Units.convert(eval(~context, a), ~fromUnits, ~toUnits)
  }
and createEvalCb = (~context, body, x) =>
  eval(~context=Belt.Map.String.set(context, "x", x), body)
and evalScalar = (~context, x): Scalar.t =>
  switch (eval(~context, x)) {
  | #Scalar.t as s => s
  | _ => Scalar.nan
  };

let eval = (~context=Belt.Map.String.empty, v) => eval(~context, v);

let solveRoot = (body, initial) => {
  let fn = value => {
    let context = Belt.Map.String.empty->Belt.Map.String.set("x", value);
    eval(~context, body);
  };

  let initial = eval(initial);
  initial != `N ? Value.solveRoot(fn, initial) : `N;
};
let solveQuadratic = (a, b, c) => {
  let a = eval(a);
  let b = a != `N ? eval(b) : `N;
  let c = b != `N ? eval(c) : `N;
  c != `N ? Value.quadratic(a, b, c) : (`N, `N);
};
let solveCubic = (a, b, c, d) => {
  let a = eval(a);
  let b = a != `N ? eval(b) : `N;
  let c = b != `N ? eval(c) : `N;
  let d = c != `N ? eval(d) : `N;
  d != `N ? Value.cubic(a, b, c, d) : (`N, `N, `N);
};
let solveVar2 = (x0, y0, c0, x1, y1, c1) => {
  let x0 = eval(x0);
  let y0 = x0 != `N ? eval(y0) : `N;
  let c0 = y0 != `N ? eval(c0) : `N;
  let x1 = c0 != `N ? eval(x1) : `N;
  let y1 = x1 != `N ? eval(y1) : `N;
  let c1 = y1 != `N ? eval(c1) : `N;
  c1 != `N ? Value.var2(x0, y0, c0, x1, y1, c1) : (`N, `N);
};
let solveVar3 = (x0, y0, z0, c0, x1, y1, z1, c1, x2, y2, z2, c2) => {
  let x0 = eval(x0);
  let y0 = x0 != `N ? eval(y0) : `N;
  let c0 = y0 != `N ? eval(c0) : `N;
  let z0 = c0 != `N ? eval(z0) : `N;
  let x1 = z0 != `N ? eval(x1) : `N;
  let y1 = x1 != `N ? eval(y1) : `N;
  let z1 = y1 != `N ? eval(z1) : `N;
  let c1 = z1 != `N ? eval(c1) : `N;
  let x2 = c1 != `N ? eval(x2) : `N;
  let y2 = x2 != `N ? eval(y2) : `N;
  let z2 = y2 != `N ? eval(z2) : `N;
  let c2 = z2 != `N ? eval(c2) : `N;
  c2 != `N
    ? Value.var3(x0, y0, z0, c0, x1, y1, z1, c1, x2, y2, z2, c2)
    : (`N, `N, `N);
};
