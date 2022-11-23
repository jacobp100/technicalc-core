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
  | Fn_Trace
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
