module Node = TechniCalcCalculator.AST_Types

let withSuperscript = (value, superscript) =>
  switch superscript {
  | Some({AST.superscriptBody: superscriptBody}) => Node.Pow(value, superscriptBody)
  | None => value
  }

let handleGenericFunction = (arg, fn) =>
  switch fn {
  | AST.Sin => Node.Sin(arg)
  | Asin => Asin(arg)
  | Cosec => Cosec(arg)
  | Sinh => Sinh(arg)
  | Asinh => Asinh(arg)
  | Cos => Cos(arg)
  | Acos => Acos(arg)
  | Sec => Sec(arg)
  | Cosh => Cosh(arg)
  | Acosh => Acosh(arg)
  | Tan => Tan(arg)
  | Atan => Atan(arg)
  | Cot => Cot(arg)
  | Tanh => Tanh(arg)
  | Atanh => Atanh(arg)
  | Deg => ToDeg(arg)
  | Grad => ToGrad(arg)
  | Log => Log(arg)
  | Rad => ToRad(arg)
  | Re => Re(arg)
  | Im => Im(arg)
  | Gamma => Gamma(arg)
  }

let handleFunction = (fn, body) =>
  switch fn {
  | Value_Types.GenericFunction({func, resultSuperscript}) =>
    let value = handleGenericFunction(body, func)
    switch resultSuperscript {
    | Some(resultSuperscript) => Node.Pow(value, resultSuperscript)
    | None => value
    }
  | NLog({base}) => LogBase({base: base, body: body})
  | Sum({from, to_}) => Sum({from: from, to_: to_, body: body})
  | Product({from, to_}) => Product({from: from, to_: to_, body: body})
  }

let handleOp = (op, a, b) =>
  switch op {
  | AST.Add => Node.Add(a, b)
  | Sub => Sub(a, b)
  | Mul => Mul(a, b)
  | Div => Div(a, b)
  | Dot => Dot(a, b)
  }
