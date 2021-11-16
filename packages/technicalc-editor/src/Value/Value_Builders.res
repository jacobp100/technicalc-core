module Node = TechniCalcCalculator.AST_Types

let withSuperscript = (value, superscript) =>
  switch superscript {
  | Some({AST.superscriptBody: superscriptBody}) => Node.Pow(value, superscriptBody)
  | None => value
  }

let handleGenericFunction = (arg, fn) =>
  switch fn {
  | AST.Fn_Sin => Node.Sin(arg)
  | Fn_Asin => Asin(arg)
  | Fn_Cosec => Cosec(arg)
  | Fn_Sinh => Sinh(arg)
  | Fn_Asinh => Asinh(arg)
  | Fn_Cos => Cos(arg)
  | Fn_Acos => Acos(arg)
  | Fn_Sec => Sec(arg)
  | Fn_Cosh => Cosh(arg)
  | Fn_Acosh => Acosh(arg)
  | Fn_Tan => Tan(arg)
  | Fn_Atan => Atan(arg)
  | Fn_Cot => Cot(arg)
  | Fn_Tanh => Tanh(arg)
  | Fn_Atanh => Atanh(arg)
  | Fn_Deg => ToDeg(arg)
  | Fn_Grad => ToGrad(arg)
  | Fn_Log => Log(arg)
  | Fn_Rad => ToRad(arg)
  | Fn_Re => Re(arg)
  | Fn_Im => Im(arg)
  | Fn_Gamma => Gamma(arg)
  }

let handleFunction = (fn, body) =>
  switch fn {
  | Value_Types.GenericFunction({fn, resultSuperscript}) =>
    let value = handleGenericFunction(body, fn)
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
  | AST.Op_Add => Node.Add(a, b)
  | Op_Sub => Sub(a, b)
  | Op_Mul => Mul(a, b)
  | Op_Div => Div(a, b)
  | Op_Dot => Dot(a, b)
  | Op_Rem => Rem(a, b)
  }
