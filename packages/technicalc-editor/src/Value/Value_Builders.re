module Node = TechniCalcCalculator.AST_Types;

let withSuperscript = (value, superscript) =>
  switch (superscript) {
  | Some(AST.{superscriptBody}) => Node.Pow(value, superscriptBody)
  | None => value
  };

let handleGenericFunction = (arg, fn) =>
  switch (fn) {
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
  | Log => Log(arg)
  | Re => Re(arg)
  | Im => Im(arg)
  | Gamma => Gamma(arg)
  };

let handleFunction = (body, fn) =>
  switch (fn) {
  | Value_Types.GenericFunction({func, squareResultSuperscript}) =>
    let value = handleGenericFunction(body, func);
    switch (squareResultSuperscript) {
    | Some(squareResultSuperscript) =>
      Node.Pow(value, squareResultSuperscript)
    | None => value
    };
  | NLog({base}) => Div(Log(body), Log(base))
  | Sum({from, to_}) => Sum({from, to_, body})
  | Product({from, to_}) => Product({from, to_, body})
  };

let handleOp = (op, a, b) =>
  switch (op) {
  | AST.Add => Node.Add(a, b)
  | Sub => Sub(a, b)
  | Mul => Mul(a, b)
  | Div => Div(a, b)
  | Dot => Dot(a, b)
  };
