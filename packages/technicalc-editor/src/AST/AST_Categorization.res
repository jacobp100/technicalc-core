open AST_Types

type base =
  | Base_Bin
  | Base_Oct
  | Base_Hex
type fn =
  | Fn_Acos
  | Fn_Acosh
  | Fn_Asin
  | Fn_Asinh
  | Fn_Atan
  | Fn_Atanh
  | Fn_Deg
  | Fn_Gamma
  | Fn_Grad
  | Fn_Im
  | Fn_Log
  | Fn_Rad
  | Fn_Re
  | Fn_Rref
  /* Superscripts supported */
  | Fn_Cos
  | Fn_Cosec
  | Fn_Cosh
  | Fn_Cot
  | Fn_Sec
  | Fn_Sin
  | Fn_Sinh
  | Fn_Tan
  | Fn_Tanh
type angle =
  | Angle_Radian
  | Angle_Degree
  | Angle_ArcMinute
  | Angle_ArcSecond
  | Angle_Gradian
type op =
  | Op_Add
  | Op_Sub
  | Op_Mul
  | Op_Div
  | Op_Dot
  | Op_Rem

let elementToOp = (element: t): option<op> =>
  switch element {
  | Add => Some(Op_Add)
  | Sub => Some(Op_Sub)
  | Mul => Some(Op_Mul)
  | Div => Some(Op_Div)
  | Dot => Some(Op_Dot)
  | Rem => Some(Op_Rem)
  | _ => None
  }

let opToElement = (element: op): t =>
  switch element {
  | Op_Add => Add
  | Op_Sub => Sub
  | Op_Mul => Mul
  | Op_Div => Div
  | Op_Dot => Dot
  | Op_Rem => Rem
  }

let elementToFn = (element: t): option<fn> =>
  switch element {
  | Acos => Some(Fn_Acos)
  | Acosh => Some(Fn_Acosh)
  | Asin => Some(Fn_Asin)
  | Asinh => Some(Fn_Asinh)
  | Atan => Some(Fn_Atan)
  | Atanh => Some(Fn_Atanh)
  | DegFunction => Some(Fn_Deg)
  | Gamma => Some(Fn_Gamma)
  | GradFunction => Some(Fn_Grad)
  | Im => Some(Fn_Im)
  | Log => Some(Fn_Log)
  | RadFunction => Some(Fn_Rad)
  | Re => Some(Fn_Re)
  | CosS => Some(Fn_Cos)
  | CosecS => Some(Fn_Cosec)
  | CoshS => Some(Fn_Cosh)
  | CotS => Some(Fn_Cot)
  | SecS => Some(Fn_Sec)
  | SinS => Some(Fn_Sin)
  | SinhS => Some(Fn_Sinh)
  | TanS => Some(Fn_Tan)
  | TanhS => Some(Fn_Tanh)
  | _ => None
  }

let fnToElement = (element: fn): t =>
  switch element {
  | Fn_Acos => Acos
  | Fn_Acosh => Acosh
  | Fn_Asin => Asin
  | Fn_Asinh => Asinh
  | Fn_Atan => Atan
  | Fn_Atanh => Atanh
  | Fn_Deg => DegFunction
  | Fn_Gamma => Gamma
  | Fn_Grad => GradFunction
  | Fn_Im => Im
  | Fn_Log => Log
  | Fn_Rad => RadFunction
  | Fn_Re => Re
  | Fn_Rref => Rref
  | Fn_Cos => CosS
  | Fn_Cosec => CosecS
  | Fn_Cosh => CoshS
  | Fn_Cot => CotS
  | Fn_Sec => SecS
  | Fn_Sin => SinS
  | Fn_Sinh => SinhS
  | Fn_Tan => TanS
  | Fn_Tanh => TanhS
  }
