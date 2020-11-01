module Node = TechniCalcCalculator.AST_Base;

let withSuperscript = (value, superscript) =>
  switch (superscript) {
  | Some(AST.{superscriptBody}) => Node.pow(value, superscriptBody)
  | None => value
  };

let handleGenericFunction = (arg, fn) =>
  switch (fn) {
  | AST.Sin => Node.sin(arg)
  | Asin => Node.asin(arg)
  | Sinh => Node.sinh(arg)
  | Asinh => Node.asinh(arg)
  | Cos => Node.cos(arg)
  | Acos => Node.acos(arg)
  | Cosh => Node.cosh(arg)
  | Acosh => Node.acosh(arg)
  | Tan => Node.tan(arg)
  | Atan => Node.atan(arg)
  | Tanh => Node.tanh(arg)
  | Atanh => Node.atanh(arg)
  | Log => Node.log(arg)
  | Re => Node.re(arg)
  | Im => Node.im(arg)
  | Gamma => Node.gamma(arg)
  };

let handleFunction = (arg, fn) =>
  switch (fn) {
  | Value_Types.GenericFunction({func, squareResultSuperscript}) =>
    let value = handleGenericFunction(arg, func);
    switch (squareResultSuperscript) {
    | Some(squareResultSuperscript) =>
      Node.pow(value, squareResultSuperscript)
    | None => value
    };
  | NLog({base}) => Node.div(Node.log(arg), Node.log(base))
  | Sum({start, end_}) => Node.sum(start, end_, arg)
  | Product({start, end_}) => Node.product(start, end_, arg)
  };

let handleOp = (op, a, b) =>
  switch (op) {
  | AST.Add => Node.add(a, b)
  | Sub => Node.sub(a, b)
  | Mul => Node.mul(a, b)
  | Div => Node.div(a, b)
  | Dot => Node.dot(a, b)
  };
