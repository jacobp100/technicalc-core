open AST_Types

%%private(
  let toRad = (~config, value): Value.t =>
    switch config.angleMode {
    | Degree => Value.ofDeg(value)
    | Gradian => Value.ofGrad(value)
    | Radian => value
    }
)

%%private(
  let ofRad = (~config, value): Value.t =>
    switch config.angleMode {
    | Degree => Value.toDeg(value)
    | Gradian => Value.toGrad(value)
    | Radian => value
    }
)

let rec eval = (~config, ~context, ~x, node: t): Value.t =>
  switch node {
  | NaN => #N
  | Zero => Value.zero
  | One => Value.one
  | MinusOne => Value.minusOne
  | I => Value.i
  | MinusI => Value.minusI
  | Pi => Value.pi
  | E => Value.e
  | OfInt(a) => Value.ofInt(a)
  | OfFloat(a) => Value.ofFloat(a)
  | OfString(a) => Value.ofString(a)->Belt.Option.getWithDefault(#N)
  | OfStringBase(base, a) => Value.ofStringBase(base, a)->Belt.Option.getWithDefault(#N)
  | Percent(percent) => evalScalar(~config, ~context, ~x, percent)->Value.ofPercent
  | Vector(elements) => Value.ofVector(Belt.Array.map(elements, evalScalar(~config, ~context, ~x)))
  | Matrix({numRows, numColumns, elements}) =>
    {
      open Matrix
      {
        numRows: numRows,
        numColumns: numColumns,
        elements: Belt.Array.map(elements, evalScalar(~config, ~context, ~x)),
      }
    }->Value.ofMatrix
  | OfEncoded(a) => Value.decode(a)->Belt.Option.getWithDefault(#N)
  | Variable(ident) => AST_Context.get(context, ident)->Belt.Option.getWithDefault(#N)
  | Add(a, b) => Value.add(eval(~config, ~context, ~x, a), eval(~config, ~context, ~x, b))
  | Sub(a, b) => Value.sub(eval(~config, ~context, ~x, a), eval(~config, ~context, ~x, b))
  | Mul(a, b) => Value.mul(eval(~config, ~context, ~x, a), eval(~config, ~context, ~x, b))
  | Div(a, b) => Value.div(eval(~config, ~context, ~x, a), eval(~config, ~context, ~x, b))
  | Pow(a, b) => Value.pow(eval(~config, ~context, ~x, a), eval(~config, ~context, ~x, b))
  | Dot(a, b) => Value.dot(eval(~config, ~context, ~x, a), eval(~config, ~context, ~x, b))
  | Neg(a) => Value.neg(eval(~config, ~context, ~x, a))
  | Abs(a) => Value.abs(eval(~config, ~context, ~x, a))
  | Floor(a) => Value.floor(eval(~config, ~context, ~x, a))
  | Ceil(a) => Value.ceil(eval(~config, ~context, ~x, a))
  | Round(a) => Value.round(eval(~config, ~context, ~x, a))
  | Sqrt(a) => Value.sqrt(eval(~config, ~context, ~x, a))
  | Exp(a) => Value.exp(eval(~config, ~context, ~x, a))
  | Log(a) => Value.log(eval(~config, ~context, ~x, a))
  | Sin(a) => eval(~config, ~context, ~x, a)->toRad(~config, _)->Value.sin
  | Cosec(a) => eval(~config, ~context, ~x, a)->toRad(~config, _)->Value.cosec
  | Sinh(a) => eval(~config, ~context, ~x, a)->toRad(~config, _)->Value.sinh
  | Cos(a) => eval(~config, ~context, ~x, a)->toRad(~config, _)->Value.cos
  | Sec(a) => eval(~config, ~context, ~x, a)->toRad(~config, _)->Value.sec
  | Cosh(a) => eval(~config, ~context, ~x, a)->toRad(~config, _)->Value.cosh
  | Tan(a) => eval(~config, ~context, ~x, a)->toRad(~config, _)->Value.tan
  | Cot(a) => eval(~config, ~context, ~x, a)->toRad(~config, _)->Value.cot
  | Tanh(a) => eval(~config, ~context, ~x, a)->toRad(~config, _)->Value.tanh
  | Asin(a) => eval(~config, ~context, ~x, a)->Value.asin->ofRad(~config, _)
  | Asinh(a) => eval(~config, ~context, ~x, a)->Value.asinh->ofRad(~config, _)
  | Acos(a) => eval(~config, ~context, ~x, a)->Value.acos->ofRad(~config, _)
  | Acosh(a) => eval(~config, ~context, ~x, a)->Value.acosh->ofRad(~config, _)
  | Atan(a) => eval(~config, ~context, ~x, a)->Value.atan->ofRad(~config, _)
  | Atanh(a) => eval(~config, ~context, ~x, a)->Value.atanh->ofRad(~config, _)
  | ToRad(a) => eval(~config, ~context, ~x, a)->toRad(~config, _)
  | ToDeg(a) => eval(~config, ~context, ~x, a)->toRad(~config, _)->Value.toDeg
  | ToGrad(a) => eval(~config, ~context, ~x, a)->toRad(~config, _)->Value.toGrad
  | OfRad(a) => eval(~config, ~context, ~x, a)->ofRad(~config, _)
  | OfDeg(a) => eval(~config, ~context, ~x, a)->Value.ofDeg->ofRad(~config, _)
  | OfArcMin(a) => eval(~config, ~context, ~x, a)->Value.ofArcMin->ofRad(~config, _)
  | OfArcSec(a) => eval(~config, ~context, ~x, a)->Value.ofArcSec->ofRad(~config, _)
  | OfGrad(a) => eval(~config, ~context, ~x, a)->Value.ofGrad->ofRad(~config, _)
  | Re(a) => Value.re(eval(~config, ~context, ~x, a))
  | Im(a) => Value.im(eval(~config, ~context, ~x, a))
  | Conj(a) => Value.conj(eval(~config, ~context, ~x, a))
  | Gamma(a) => Value.gamma(eval(~config, ~context, ~x, a))
  | Factorial(a) => Value.factorial(eval(~config, ~context, ~x, a))
  | Rand => Value.rand()
  | RandInt(a, b) => Value.randInt(eval(~config, ~context, ~x, a), eval(~config, ~context, ~x, b))
  | NPR(a, b) => Value.nPr(eval(~config, ~context, ~x, a), eval(~config, ~context, ~x, b))
  | NCR(a, b) => Value.nCr(eval(~config, ~context, ~x, a), eval(~config, ~context, ~x, b))
  | Min(a, b) => Value.min(eval(~config, ~context, ~x, a), eval(~config, ~context, ~x, b))
  | Max(a, b) => Value.max(eval(~config, ~context, ~x, a), eval(~config, ~context, ~x, b))
  | Gcd(a, b) => Value.gcd(eval(~config, ~context, ~x, a), eval(~config, ~context, ~x, b))
  | Lcm(a, b) => Value.lcm(eval(~config, ~context, ~x, a), eval(~config, ~context, ~x, b))
  | IteratorX => x
  | Differential({at, body}) =>
    Value.derivative(createEvalCb(~config, ~context, body), eval(~config, ~context, ~x, at))
  | Integral({from, to_, body}) =>
    Value.integrate(
      createEvalCb(~config, ~context, body),
      eval(~config, ~context, ~x, from),
      eval(~config, ~context, ~x, to_),
    )
  | Sum({from, to_, body}) =>
    Value.sum(
      createEvalCb(~config, ~context, body),
      eval(~config, ~context, ~x, from),
      eval(~config, ~context, ~x, to_),
    )
  | Product({from, to_, body}) =>
    Value.product(
      createEvalCb(~config, ~context, body),
      eval(~config, ~context, ~x, from),
      eval(~config, ~context, ~x, to_),
    )
  | Convert({body, fromUnits, toUnits}) =>
    Units.convert(eval(~config, ~context, ~x, body), ~fromUnits, ~toUnits)
  }
and createEvalCb = (~config, ~context, body, x) => eval(~config, ~context, ~x, body)
and evalScalar = (~config, ~context, ~x, body): Scalar.t =>
  switch eval(~config, ~context, ~x, body) {
  | #...Scalar.t as s => s
  | _ => Scalar.nan
  }

let eval = (~context, ~config, v) => eval(~config, ~context, ~x=Value.nan, v)

let solveRoot = (~config, ~context, body, initial) => {
  let fn = value => {
    let context = {
      open AST_Context
      set(empty, "x", value)
    }
    eval(~config, ~context, body)
  }

  let initial = eval(~config, ~context, initial)
  initial != #N ? Value.solveRoot(fn, initial) : #N
}
let solveQuadratic = (~config, ~context, a, b, c) => {
  let a = eval(~config, ~context, a)
  let b = a != #N ? eval(~config, ~context, b) : #N
  let c = b != #N ? eval(~config, ~context, c) : #N
  c != #N ? Value.quadratic(a, b, c) : (#N, #N)
}
let solveCubic = (~config, ~context, a, b, c, d) => {
  let a = eval(~config, ~context, a)
  let b = a != #N ? eval(~config, ~context, b) : #N
  let c = b != #N ? eval(~config, ~context, c) : #N
  let d = c != #N ? eval(~config, ~context, d) : #N
  d != #N ? Value.cubic(a, b, c, d) : (#N, #N, #N)
}
let solveVar2 = (~config, ~context, x0, y0, c0, x1, y1, c1) => {
  let x0 = eval(~config, ~context, x0)
  let y0 = x0 != #N ? eval(~config, ~context, y0) : #N
  let c0 = y0 != #N ? eval(~config, ~context, c0) : #N
  let x1 = c0 != #N ? eval(~config, ~context, x1) : #N
  let y1 = x1 != #N ? eval(~config, ~context, y1) : #N
  let c1 = y1 != #N ? eval(~config, ~context, c1) : #N
  c1 != #N ? Value.var2(x0, y0, c0, x1, y1, c1) : (#N, #N)
}
let solveVar3 = (~config, ~context, x0, y0, z0, c0, x1, y1, z1, c1, x2, y2, z2, c2) => {
  let x0 = eval(~config, ~context, x0)
  let y0 = x0 != #N ? eval(~config, ~context, y0) : #N
  let c0 = y0 != #N ? eval(~config, ~context, c0) : #N
  let z0 = c0 != #N ? eval(~config, ~context, z0) : #N
  let x1 = z0 != #N ? eval(~config, ~context, x1) : #N
  let y1 = x1 != #N ? eval(~config, ~context, y1) : #N
  let z1 = y1 != #N ? eval(~config, ~context, z1) : #N
  let c1 = z1 != #N ? eval(~config, ~context, c1) : #N
  let x2 = c1 != #N ? eval(~config, ~context, x2) : #N
  let y2 = x2 != #N ? eval(~config, ~context, y2) : #N
  let z2 = y2 != #N ? eval(~config, ~context, z2) : #N
  let c2 = z2 != #N ? eval(~config, ~context, c2) : #N
  c2 != #N ? Value.var3(x0, y0, z0, c0, x1, y1, z1, c1, x2, y2, z2, c2) : (#N, #N, #N)
}
