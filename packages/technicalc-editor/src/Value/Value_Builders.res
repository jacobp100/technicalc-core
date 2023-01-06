module Node = TechniCalcCalculator.AST_Types

let withSuperscript = (value, superscript) =>
  switch superscript {
  | Some({AST.superscriptBody: superscriptBody}) => Node.Pow(value, superscriptBody)
  | None => value
  }

let applyAngle = (value: Value_Types.node, angle: AST.angle): Value_Types.node =>
  switch angle {
  | Angle_Radian => OfRad(value)
  | Angle_Degree => OfDeg(value)
  | Angle_ArcMinute => OfArcMin(value)
  | Angle_ArcSecond => OfArcSec(value)
  | Angle_Gradian => OfGrad(value)
  }

let applyFunction = (fn, arg) =>
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
  | Fn_Rref => Rref(arg)
  | Fn_Trace => Trace(arg)
  | Fn_Gamma => Gamma(arg)
  | Fn_NLog({base}) => LogBase({base, body: arg})
  | Fn_Sum({from, to}) => Sum({from, to, body: arg})
  | Fn_Product({from, to}) => Product({from, to, body: arg})
  }
