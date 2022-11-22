open AST_Types

%%private(
  let toScalarOrNan = (value: Value.t): Scalar.t =>
    switch value {
    | #...Scalar.t as scalar => scalar
    | _ => Scalar.nan
    }
)

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

let rec evalAt = (~config, ~context, ~x, node: t): Value.t =>
  switch node {
  | NaN => Value.nan
  | Zero => Value.zero
  | One => Value.one
  | MinusOne => Value.minusOne
  | I => Value.i
  | MinusI => Value.minusI
  | Pi => Value.pi
  | E => Value.e
  | OfInt(a) => Value.ofInt(a)
  | OfFloat(a) => Value.ofFloat(a)
  | OfString(a) => Formatting.ofString(a)->Belt.Option.getWithDefault(Value.nan)
  | OfStringBase(base, a) => Formatting.ofStringBase(base, a)->Belt.Option.getWithDefault(Value.nan)
  | Percent(percent) => evalAt(~config, ~context, ~x, percent)->toScalarOrNan->Value.ofPercent
  | Vector(elements) =>
    let vector = Vector.makeByU(Belt.Array.length(elements), (. i) => {
      let a = Belt.Array.getUnsafe(elements, i)
      evalAt(~config, ~context, ~x, a)->toScalarOrNan
    })
    Value.ofVector(vector)
  | Matrix({numRows, numColumns, elements}) =>
    let matrix = Matrix.makeByIndexU(~numRows, ~numColumns, (. i) => {
      let a = Belt.Array.getUnsafe(elements, i)
      evalAt(~config, ~context, ~x, a)->toScalarOrNan
    })
    Value.ofMatrix(matrix)
  | OfEncoded(a) => Encoding.decode(a)->Belt.Option.getWithDefault(Value.nan)
  | Variable(ident) => AST_Context.get(context, ident)->Belt.Option.getWithDefault(Value.nan)
  | Add(a, b) => Value.add(evalAt(~config, ~context, ~x, a), evalAt(~config, ~context, ~x, b))
  | Sub(a, b) => Value.sub(evalAt(~config, ~context, ~x, a), evalAt(~config, ~context, ~x, b))
  | Mul(a, b) => Value.mul(evalAt(~config, ~context, ~x, a), evalAt(~config, ~context, ~x, b))
  | Div(a, b) => Value.div(evalAt(~config, ~context, ~x, a), evalAt(~config, ~context, ~x, b))
  | Pow(a, b) => Value.pow(evalAt(~config, ~context, ~x, a), evalAt(~config, ~context, ~x, b))
  | Dot(a, b) => Value.dot(evalAt(~config, ~context, ~x, a), evalAt(~config, ~context, ~x, b))
  | Neg(a) => Value.neg(evalAt(~config, ~context, ~x, a))
  | Abs(a) => Value.abs(evalAt(~config, ~context, ~x, a))
  | Floor(a) => Value.floor(evalAt(~config, ~context, ~x, a))
  | Ceil(a) => Value.ceil(evalAt(~config, ~context, ~x, a))
  | Round(a) => Value.round(evalAt(~config, ~context, ~x, a))
  | Sqrt(a) => Value.sqrt(evalAt(~config, ~context, ~x, a))
  | Exp(a) => Value.exp(evalAt(~config, ~context, ~x, a))
  | Log(a) => Value.log(evalAt(~config, ~context, ~x, a))
  | LogBase({base, body}) =>
    Value.logBase(~base=evalAt(~config, ~context, ~x, base), evalAt(~config, ~context, ~x, body))
  | Sin(a) => evalAt(~config, ~context, ~x, a)->toRad(~config, _)->Value.sin
  | Cosec(a) => evalAt(~config, ~context, ~x, a)->toRad(~config, _)->Value.cosec
  | Sinh(a) => evalAt(~config, ~context, ~x, a)->toRad(~config, _)->Value.sinh
  | Cos(a) => evalAt(~config, ~context, ~x, a)->toRad(~config, _)->Value.cos
  | Sec(a) => evalAt(~config, ~context, ~x, a)->toRad(~config, _)->Value.sec
  | Cosh(a) => evalAt(~config, ~context, ~x, a)->toRad(~config, _)->Value.cosh
  | Tan(a) => evalAt(~config, ~context, ~x, a)->toRad(~config, _)->Value.tan
  | Cot(a) => evalAt(~config, ~context, ~x, a)->toRad(~config, _)->Value.cot
  | Tanh(a) => evalAt(~config, ~context, ~x, a)->toRad(~config, _)->Value.tanh
  | Asin(a) => evalAt(~config, ~context, ~x, a)->Value.asin->ofRad(~config, _)
  | Asinh(a) => evalAt(~config, ~context, ~x, a)->Value.asinh->ofRad(~config, _)
  | Acos(a) => evalAt(~config, ~context, ~x, a)->Value.acos->ofRad(~config, _)
  | Acosh(a) => evalAt(~config, ~context, ~x, a)->Value.acosh->ofRad(~config, _)
  | Atan(a) => evalAt(~config, ~context, ~x, a)->Value.atan->ofRad(~config, _)
  | Atanh(a) => evalAt(~config, ~context, ~x, a)->Value.atanh->ofRad(~config, _)
  | ToRad(a) => evalAt(~config, ~context, ~x, a)->toRad(~config, _)
  | ToDeg(a) => evalAt(~config, ~context, ~x, a)->toRad(~config, _)->Value.toDeg
  | ToGrad(a) => evalAt(~config, ~context, ~x, a)->toRad(~config, _)->Value.toGrad
  | OfRad(a) => evalAt(~config, ~context, ~x, a)->ofRad(~config, _)
  | OfDeg(a) => evalAt(~config, ~context, ~x, a)->Value.ofDeg->ofRad(~config, _)
  | OfArcMin(a) => evalAt(~config, ~context, ~x, a)->Value.ofArcMin->ofRad(~config, _)
  | OfArcSec(a) => evalAt(~config, ~context, ~x, a)->Value.ofArcSec->ofRad(~config, _)
  | OfGrad(a) => evalAt(~config, ~context, ~x, a)->Value.ofGrad->ofRad(~config, _)
  | Re(a) => Value.re(evalAt(~config, ~context, ~x, a))
  | Im(a) => Value.im(evalAt(~config, ~context, ~x, a))
  | Conj(a) => Value.conj(evalAt(~config, ~context, ~x, a))
  | Gamma(a) => Value.gamma(evalAt(~config, ~context, ~x, a))
  | Factorial(a) => Value.factorial(evalAt(~config, ~context, ~x, a))
  | Rand => Value.rand()
  | RandInt(a, b) =>
    Value.randInt(evalAt(~config, ~context, ~x, a), evalAt(~config, ~context, ~x, b))
  | NPR(a, b) => Value.nPr(evalAt(~config, ~context, ~x, a), evalAt(~config, ~context, ~x, b))
  | NCR(a, b) => Value.nCr(evalAt(~config, ~context, ~x, a), evalAt(~config, ~context, ~x, b))
  | Rem(a, b) => Value.rem(evalAt(~config, ~context, ~x, a), evalAt(~config, ~context, ~x, b))
  | Min(a, b) => Value.min(evalAt(~config, ~context, ~x, a), evalAt(~config, ~context, ~x, b))
  | Max(a, b) => Value.max(evalAt(~config, ~context, ~x, a), evalAt(~config, ~context, ~x, b))
  | Gcd(a, b) => Value.gcd(evalAt(~config, ~context, ~x, a), evalAt(~config, ~context, ~x, b))
  | Lcm(a, b) => Value.lcm(evalAt(~config, ~context, ~x, a), evalAt(~config, ~context, ~x, b))
  | Transpose(a) => Value.transpose(evalAt(~config, ~context, ~x, a))
  | Rref(a) => Value.rref(evalAt(~config, ~context, ~x, a))
  | X => x
  | Differential({at, body}) =>
    Value.differentiateU(
      createEvalAtFnU(~config, ~context, body),
      evalAt(~config, ~context, ~x, at),
    )
  | Integral({from, to, body}) =>
    Value.integrateU(
      createEvalAtFnU(~config, ~context, body),
      evalAt(~config, ~context, ~x, from),
      evalAt(~config, ~context, ~x, to),
    )
  | Sum({from, to, body}) =>
    Value.sumU(
      createEvalAtFnU(~config, ~context, body),
      evalAt(~config, ~context, ~x, from),
      evalAt(~config, ~context, ~x, to),
    )
  | Product({from, to, body}) =>
    Value.productU(
      createEvalAtFnU(~config, ~context, body),
      evalAt(~config, ~context, ~x, from),
      evalAt(~config, ~context, ~x, to),
    )
  | Convert({body, fromUnits, toUnits}) =>
    Units.convert(evalAt(~config, ~context, ~x, body), ~fromUnits, ~toUnits)
  }
and createEvalAtFnU = (~config, ~context, body) => {
  let fn = (. x) => evalAt(~config, ~context, ~x, body)
  fn
}

let eval = (~context, ~config, v) => evalAt(~config, ~context, ~x=Value.nan, v)

let solveRoot = (~config, ~context, body, initial) => {
  let fn = createEvalAtFnU(~config, ~context, body)
  let initial = eval(~config, ~context, initial)
  initial != #NaNN ? Solvers.solveRootU(fn, initial) : Value.nan
}
let solveQuadratic = (~config, ~context, a, b, c) => {
  let a = eval(~config, ~context, a)
  let b = a != #NaNN ? eval(~config, ~context, b) : Value.nan
  let c = b != #NaNN ? eval(~config, ~context, c) : Value.nan
  c != #NaNN ? Solvers.quadratic(a, b, c) : (Value.nan, Value.nan)
}
let solveCubic = (~config, ~context, a, b, c, d) => {
  let a = eval(~config, ~context, a)
  let b = a != #NaNN ? eval(~config, ~context, b) : Value.nan
  let c = b != #NaNN ? eval(~config, ~context, c) : Value.nan
  let d = c != #NaNN ? eval(~config, ~context, d) : Value.nan
  d != #NaNN ? Solvers.cubic(a, b, c, d) : (Value.nan, Value.nan, Value.nan)
}
let solveVar2 = (~config, ~context, x0, y0, c0, x1, y1, c1) => {
  let x0 = eval(~config, ~context, x0)
  let y0 = x0 != #NaNN ? eval(~config, ~context, y0) : Value.nan
  let c0 = y0 != #NaNN ? eval(~config, ~context, c0) : Value.nan
  let x1 = c0 != #NaNN ? eval(~config, ~context, x1) : Value.nan
  let y1 = x1 != #NaNN ? eval(~config, ~context, y1) : Value.nan
  let c1 = y1 != #NaNN ? eval(~config, ~context, c1) : Value.nan
  c1 != #NaNN ? Solvers.var2(x0, y0, c0, x1, y1, c1) : (Value.nan, Value.nan)
}
let solveVar3 = (~config, ~context, x0, y0, z0, c0, x1, y1, z1, c1, x2, y2, z2, c2) => {
  let x0 = eval(~config, ~context, x0)
  let y0 = x0 != #NaNN ? eval(~config, ~context, y0) : Value.nan
  let c0 = y0 != #NaNN ? eval(~config, ~context, c0) : Value.nan
  let z0 = c0 != #NaNN ? eval(~config, ~context, z0) : Value.nan
  let x1 = z0 != #NaNN ? eval(~config, ~context, x1) : Value.nan
  let y1 = x1 != #NaNN ? eval(~config, ~context, y1) : Value.nan
  let z1 = y1 != #NaNN ? eval(~config, ~context, z1) : Value.nan
  let c1 = z1 != #NaNN ? eval(~config, ~context, c1) : Value.nan
  let x2 = c1 != #NaNN ? eval(~config, ~context, x2) : Value.nan
  let y2 = x2 != #NaNN ? eval(~config, ~context, y2) : Value.nan
  let z2 = y2 != #NaNN ? eval(~config, ~context, z2) : Value.nan
  let c2 = z2 != #NaNN ? eval(~config, ~context, c2) : Value.nan
  c2 != #NaNN
    ? Solvers.var3(x0, y0, z0, c0, x1, y1, z1, c1, x2, y2, z2, c2)
    : (Value.nan, Value.nan, Value.nan)
}
